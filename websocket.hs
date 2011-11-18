{-# LANGUAGE OverloadedStrings, TupleSections #-}
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)

import Data.Monoid
import Data.Char (isPunctuation, isSpace)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (stripPrefix, isPrefixOf)
import Data.Maybe (Maybe, listToMaybe, fromMaybe)

import Control.Exception (fromException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, msum, foldM)
import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Data.Enumerator (Iteratee, Enumerator, run_, ($$), (=$), Stream(..), Step(..), runIteratee, joinI, (>>==) )
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Data.Attoparsec.Enumerator (iterParser)

import Blaze.ByteString.Builder (Builder, fromByteString, toByteString, fromLazyByteString)
import Blaze.ByteString.Builder.HTTP
    (chunkedTransferEncoding, chunkedTransferTerminator)

import Network.HTTP.Types-- (Status, statusOK, statusNotFound, decodePathSegments)
import Network.Wai (Application, Request(..), Response(..), pathInfo)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, Settings(..))

import Network.WebSockets hiding (Request, Response, requestHeaders, fromLazyByteString, close)
import Network.WebSockets.Emulate
import qualified Network.WebSockets as WS

import Data.FileEmbed (embedDir)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static

import TestEmulate (chat, echo, close, ServerState, writeMsg, readMsg)

chunk :: E.Enumeratee Builder Builder IO a
chunk = E.checkDone $ E.continue . step
  where
    step k E.EOF = k (E.Chunks [chunkedTransferTerminator]) >>== return
    step k (E.Chunks []) = E.continue $ step k
    step k (E.Chunks [x]) = k (E.Chunks [chunkedTransferEncoding x]) >>== chunk
    step k (E.Chunks xs) = k (E.Chunks [chunkedTransferEncoding $ mconcat xs]) >>== chunk

responseBS :: ByteString -> Response
responseBS txt = ResponseBuilder statusOK [("Content-Type", "text/plain")] (fromByteString txt)

response :: ByteString -> Response
response s = ResponseBuilder statusOK [("Content-Type", "text/plain")] (fromByteString s)

responseNotFound :: Response
responseNotFound = ResponseBuilder statusNotFound [] (fromByteString "404/resource not found")

type SessionId = Text
type Session = (StreamChan ByteString, StreamChan ByteString)
type SessionMap = Map SessionId Session

type WSApp p = WS.Request -> WebSockets p ()
type AppRoute p = [([Text], WSApp p)]

-- | create session if not exists, Left -> old session, Right -> new session.
ensureSession :: MVar SessionMap -> SessionId -> WSApp EmulateProtocol -> Request -> IO (Either Session Session)
ensureSession msm sid ws req = modifyMVar msm $ \sm ->
    case M.lookup sid sm of
        Just old -> return (sm, Left old)
        Nothing -> do
            putStrLn "new session"
            inChan <- newChan
            outChan <- newChan
            let sm' = M.insert sid (inChan, outChan) sm
            _ <- forkIO $ runEmulator inChan outChan (RequestHttpPart (rawPathInfo req) (requestHeaders req)) ws
            return (sm', Right (inChan, outChan))

getSession :: MVar SessionMap -> SessionId -> IO (Maybe Session)
getSession msm sid = M.lookup sid <$> readMVar msm

httpApp :: MVar SessionMap -> AppRoute EmulateProtocol -> Application
httpApp msm apps req = case msum $ map match apps of
    Just (app, ["chunking_test"]) -> return $ ResponseEnumerator chunkingTestEnum
    Just (app, _:sid:path) -> case path of
        ["xhr"] ->
            liftIO (ensureSession msm sid app req) >>=
            either (\(_, outChan) -> do
                      r <- fromMaybe "c[3000,\"Go away!\"]\n" <$> liftIO (readMsg outChan)
                      return $ response r
                   )
                   (\(_, outChan) -> response . S.concat . toChunks <$> liftIO (readChan outChan))
        ["xhr_send"] ->
            liftIO (getSession msm sid) >>=
            maybe (return responseNotFound)
                  (\(inChan, _) -> do
                      -- msg <- joinI $ EB.isolate len $$ EL.consume
                      msg <- EL.consume
                      liftIO $ print ("post:", msg)
                      liftIO $ writeMsg inChan $ L.fromChunks msg
                      return $ responseBS "" )
        ["xhr_streaming"] -> return $ ResponseEnumerator $ streamRsp sid app
        _ -> return responseNotFound
    _ -> return responseNotFound
  where
    match (prefix, app) = (app, ) <$> stripPrefix prefix (pathInfo req)
    toChunks (Chunks xs) = xs
    toChunks EOF = []
    streamRsp :: SessionId -> WSApp EmulateProtocol -> (Status -> Headers -> Iteratee Builder IO a) -> IO a
    streamRsp sid app f = do
        ec <- ensureSession msm sid app req
        case ec of
            Left c -> run_ $ f statusBadRequest []
            Right (_, outChan) -> do
                rsp <- S.concat . toChunks <$> readChan outChan
                print rsp
                -- TODO parse response
                let iter = EL.map fromByteString =$ f statusOK
                            [ ("Content-Type", "application/javascript; charset=UTF-8")
                            , ("Set-Cookie", "JSESSIONID=dummy; path=/")
                            , ("access-control-allow-origin", "*")
                            , ("access-control-allow-credentials", "true")
                            ]
                let iter' = enumChunks [ S.concat [S.replicate 2048 'h', "\n"]
                                    , "o\n"
                                    ] $$ iter
                let --step :: Iteratee a m b -> m (Iteratee a m b)
                    step it = do
                        mr <- readMsg outChan
                        case mr of
                            Nothing -> return $ enumSingle "c[3000,\"Go away!\"]\n" $$ it
                            Just r -> step $ enumSingle r $$ it
                step iter' >>= run_

chunkingTestEnum :: (Status -> Headers -> Iteratee Builder IO a) -> IO a
chunkingTestEnum f = do
    let iter = EL.map fromByteString =$ f statusOK
                 [ ("Content-Type", "application/javascript; charset=UTF-8")
                 , ("access-control-allow-origin", "*")
                 , ("access-control-allow-credentials", "true")
                 ]
    let iter' = enumChunks ["h\n", S.concat $ [S.replicate 2048 ' ', "h\n"]] $$ iter
    foldM (\it ms -> threadDelay (ms*1000) >> return (enumChunks ["h\n"] $$ it)) iter' [5, 25, 125, 625, 3125]
    >>= run_

runEnum :: Monad m => Enumerator a m b -> Iteratee a m b -> m (Iteratee a m b)
runEnum enum iter = runIteratee iter >>= enum

enumSingle :: Monad m => a -> Enumerator a m b
enumSingle c = enumChunks [c]

enumChunks :: Monad m => [a] -> Enumerator a m b
enumChunks xs = E.checkContinue0 $ \_ f -> f (E.Chunks xs) >>== E.returnI

wsApp :: AppRoute Hybi00 -> WSApp Hybi00
wsApp apps req = case msum $ map match apps of
    Just (app, [_,_,"websocket"]) -> app req
    _ -> WS.rejectRequest req "Forbidden!"

  where path = decodePathSegments $ WS.requestPath req
        match (prefix, app) = (app, ) <$> stripPrefix prefix path

serverState :: TextProtocol p => MVar (ServerState p)
serverState = unsafePerformIO $ newMVar M.empty

httpRoutes :: [([Text], Application)] -> Application -> Application
httpRoutes routes fallback req@Request{pathInfo=path} =
    fromMaybe (fallback req) $ msum $ map match routes
  where
    match (prefix, app) = case stripPrefix prefix path of
        Nothing -> Nothing
        Just rest -> Just $ app req{pathInfo=rest}

staticApp :: Application
staticApp = Static.staticApp Static.defaultFileServerSettings
              -- { Static.ssFolder = Static.embeddedLookup $ Static.toEmbedded $(embedDir "static") }
              { Static.ssFolder = Static.fileSystemLookup "static" }

wsRoutes :: TextProtocol p => AppRoute p
wsRoutes = [ ( ["echo"], echo )
           , ( ["chat"], chat serverState )
           , ( ["close"], close )
           ]

main :: IO ()
main = do
    port <- read . fromMaybe "3000" . listToMaybe <$> getArgs
    msm <- newMVar M.empty
    runSettings defaultSettings
           { settingsPort = port
           , settingsIntercept = WaiWS.intercept (wsApp wsRoutes)
           , settingsRechunking = False
           } $ httpRoutes [(["static"], staticApp)] (httpApp msm wsRoutes)

