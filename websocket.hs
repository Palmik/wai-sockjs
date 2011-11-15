{-# LANGUAGE OverloadedStrings, TupleSections #-}
import System.IO.Unsafe
import Data.Map (Map)
import Data.List (stripPrefix)
import qualified Data.Map as M
import Data.Unique

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import Data.Enumerator hiding (map, filter)
import Blaze.ByteString.Builder

import Control.Exception (throw)
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Concurrent

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (runSettings, defaultSettings, Settings(..))
import Network.WebSockets hiding (Request, Response)
import qualified Network.WebSockets as WS
import Network.WebSockets.Chan
import qualified Network.Wai.Handler.WebSockets as WaiWS

responseText :: Text -> Response
responseText txt = ResponseBuilder statusOK [("Content-Type", "text/plain")] (fromByteString . T.encodeUtf8 $ txt)

responseBS :: ByteString -> Response
responseBS txt = ResponseBuilder statusOK [("Content-Type", "text/plain")] (fromByteString txt)

responseNotFound :: Response
responseNotFound = ResponseBuilder statusNotFound [] (fromByteString "404/resource not found")

responseClosed :: Response
responseClosed = ResponseBuilder statusNotFound [] (fromByteString "closed")

type SessionId = Text
type Session = (StreamChan ByteString, StreamChan ByteString)
type SessionMap = Map SessionId Session

type WSApp p = WebSockets p ()
type AppRoute p = [([Text], WSApp p)]

ensureSession :: MVar SessionMap -> SessionId -> WSApp ChanProtocol -> IO Session
ensureSession msm sid ws = modifyMVar msm $ \sm -> do
    case M.lookup sid sm of
        Just old -> return (sm, old)
        Nothing -> do
            new <- (,) <$> newChan <*> newChan
            let sm' = M.insert sid new sm
            forkIO $ runWithChans (fst new) (snd new) ws
            return (sm', new)

httpApp :: MVar SessionMap -> AppRoute ChanProtocol -> Application
httpApp msm apps req = case msum $ map match apps of
    Just (app, (_:sid:path)) -> do
        (inChan, outChan) <- liftIO $ ensureSession msm sid app
        case path of
            ["xhr"] -> do
                s <- liftIO (readChan outChan)
                case s of
                    Chunks xs -> return . responseBS . S.concat $ xs
                    EOF -> return responseClosed
            ["xhr_send"] -> iterChan inChan >> return (responseBS "")
            _ -> return $ responseNotFound
    _ -> return $ responseNotFound
  where
    match (prefix, app) = (app, ) <$> stripPrefix prefix (pathInfo req)

wsApp :: AppRoute Hybi00 -> WS.Request -> WebSockets Hybi00 ()
wsApp apps req = case msum $ map match apps of
    Just (app, [_,_,"websocket"]) -> do
        acceptRequest req
        app
    _ -> rejectRequest req "Forbidden!"

  where path = decodePathSegments $ requestPath req
        match (prefix, app) = (app, ) <$> stripPrefix prefix path

echo :: TextProtocol p => WebSockets p ()
echo = forever $ do
    msg <- receiveData
    sendTextData (msg::L.ByteString)

type ServerState p = MVar (Map Unique (WS.Sink p))

broadcast :: TextProtocol p => ServerState p -> WebSockets p ()
broadcast st = do
    identity <- liftIO newUnique
    sink <- getSink
    liftIO $ modifyMVar_ st $ return . M.insert identity sink
    forever $ do
        msg <- receiveData
        clients <- filter ((/=identity) . fst) . M.toList <$> liftIO (readMVar st)
        liftIO $ forM_ clients $ \(_, sink) -> sendSink sink (textData (msg::L.ByteString))

serverState :: TextProtocol p => ServerState p
serverState = unsafePerformIO $ newMVar M.empty

apps :: TextProtocol p => AppRoute p
apps = [ ( ["echo"], echo )
       , ( ["broadcast"], broadcast serverState )
       ]

-- newApps :: IO (forall p. TextProtocol p => AppRoute p)
-- newApps = do
--     st <- newMVar M.empty
--     return [ ( ["echo"], echo )
--            , ( ["broadcast"], broadcast st )
--            ]

main :: IO ()
main = do
    -- apps <- newApps
    msm <- newMVar M.empty
    runSettings defaultSettings
           { settingsPort = 8080
           , settingsIntercept = WaiWS.intercept (wsApp apps)
           } $ httpApp msm apps

