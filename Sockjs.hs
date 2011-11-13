{-# LANGUAGE OverloadedStrings, ViewPatterns, TupleSections #-}
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

import Data.List
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.CaseInsensitive as CI
import Blaze.ByteString.Builder (fromByteString)
import Data.Enumerator (Iteratee, ($$), joinI )
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB

import Control.Applicative ( (<$>), liftA2 )
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll, recv)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import Chat (Client, Message, ServerM, TServerState)
import qualified Chat as Chat

(&&.) = liftA2 (&&)

type SessionId = Text
type ClientMap = Map Text (TChan Text)

clients :: TVar ClientMap
clients = unsafePerformIO $ newTVarIO M.empty

responseNotFound :: Response
responseNotFound = ResponseBuilder statusNotFound [] (fromByteString "404/resource not found")

responseText :: Text -> Response
responseText txt = ResponseBuilder statusOK [("Content-Type", "text/plain; charset=UTF-8")] (fromByteString . encodeUtf8 $ txt)

responseHTML :: Text -> Response
responseHTML txt = ResponseBuilder statusOK [("Content-Type", "text/html; charset=UTF-8")] (fromByteString . encodeUtf8 $ txt)

type InnerApp = Client -> Message -> ServerM ()

echoApp :: InnerApp
echoApp c m = Chat.echo c m

intercept :: TServerState -> [([Text], InnerApp)] -> Request -> Maybe (Socket -> Handle -> Integer -> Iteratee ByteString IO ())
intercept st apps req = case findapp (pathInfo req) apps of
    Nothing -> Nothing
    Just (app, path) -> Just $ intercept' app path
  where
    intercept' :: InnerApp -> [Text] -> Socket -> Handle -> Integer -> Iteratee ByteString IO ()
    intercept' app path sock th len = do
        -- msg <- T.decodeUtf8 . S.concat <$> EL.consume
        -- TODO receive post correctly.
        -- msg <- T.decodeUtf8 <$> liftIO (recv sock 4096)
        msg <- T.decodeUtf8 . S.concat <$> (joinI $ EB.isolate len $$ EL.consume)
        liftIO $ Chat.runServerM (sockjsApp app path msg) st
      where 
        -- TODO
        -- #. timeout when not having a receiving connection.
        -- #. forbid duplicate receiving connection.
        -- #. heartbeat message.
        sockjsApp :: InnerApp -> [Text] -> Message -> ServerM ()
        sockjsApp app path msg = case path of
            [] -> liftIO $ sendRsp $ responseText "Welcome to SockJS!\n"
            [isIFrame -> True] -> liftIO $ sendRsp iframeResponse
            (splitAt 2 -> ([_,sid], rest)) -> case rest of
                ["xhr"] -> Chat.recvOrJoin sid >>= either
                               (const $ liftIO $ sendRsp $ responseText "o\n")
                               (liftIO . sendRsp . responseText . T.append "a")
                ["xhr_send"] -> Chat.getClient sid >>= maybe
                                  (liftIO $ sendRsp responseNotFound)
                                  (\c -> app c msg >> liftIO (sendRsp (responseText "")))
                ["xhr_streaming"] -> do
                    mc <- Chat.getClient sid
                    case mc of
                        Just c -> liftIO $ sendRsp $ responseText "c[2010,\"Another connection still open\"]\n"
                        Nothing -> do
                            liftIO $ sendRsp (ResponseBuilder
                                                         statusOK
                                                         [("Content-Type", "application/javascript; charset=UTF-8")]
                                                         (fromByteString $ S.replicate 2048 'h'))
                            liftIO $ sendText "o\n"
                            ch <- snd . Chat.transport <$> Chat.join sid
                            forever $ do
                                msg <- Chat.liftSTM $ readTChan ch
                                liftIO $ sendText msg

                ["websocket"] -> liftIO $ sendRsp $ responseText "don't support\n"
                ["eventsource"] -> liftIO $ sendRsp $ responseText "don't support!\n"
                ["htmlfile"] -> liftIO $ sendRsp $ responseText "don't support!\n"

                ["jsonp"] -> liftIO $ sendRsp $ responseText "don't support!\n"
                ["jsonp_send"] -> liftIO $ sendRsp $ responseText "don't support!\n"
                _ -> liftIO $ sendRsp responseNotFound
            _ -> liftIO $ sendRsp responseNotFound

        sendText :: Text -> IO ()
        sendText txt = do
            resume th
            sendAll sock (T.encodeUtf8 txt)
            pause th

        sendRsp :: Response -> IO ()
        sendRsp rsp = do
            resume th
            sendResponse th req sock rsp
            pause th

    isIFrame :: Text -> Bool
    isIFrame = T.isPrefixOf "iframe" &&. T.isSuffixOf ".html"

    findapp :: [Text] -> [([Text], a)] -> Maybe (a, [Text])
    findapp path apps = msum $ flip map apps $ \(pre, app) ->
                            stripPrefix pre path >>= return . (app,)


pong :: Application
pong _ = return $ ResponseBuilder statusOK [] (fromByteString "pong")

main :: IO ()
main = do
    st <- Chat.newServerState
    port <- read . fromMaybe "3000" . listToMaybe <$> getArgs
    let sockjs = intercept st [(["echo"], echoApp)]
    runSettings defaultSettings{ settingsPort=port, settingsIntercept=sockjs } pong

iframeResponse = responseHTML iframeContent
iframeContent = "<!DOCTYPE html>\n\
\<html>\n\
\<head>\n\
\  <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\" />\n\
\  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />\n\
\  <script>\n\
\    document.domain = document.domain;\n\
\    _sockjs_onload = function(){SockJS.bootstrap_iframe();};\n\
\  </script>\n\
\  <script src=\"(?P<sockjs_url>[^\"]*)\"></script>\n\
\</head>\n\
\<body>\n\
\  <h2>Don't panic!</h2>\n\
\  <p>This is a SockJS hidden iframe. It's used for cross domain magic.</p>\n\
\</body>\n\
\</html>"
