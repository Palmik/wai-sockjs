{-# LANGUAGE OverloadedStrings, TupleSections #-}
import System.IO.Unsafe
import Data.Map (Map)
import Data.List (stripPrefix)
import qualified Data.Map as M
import Data.Unique

import Data.Attoparsec.Enumerator (iterParser)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Enumerator hiding (map, filter)
import qualified Data.Enumerator.List as EL
import Blaze.ByteString.Builder

import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Concurrent

import Network.HTTP.Types
import Network.Wai hiding (responseLBS)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, Settings(..))
import Network.WebSockets hiding (Request, Response, fromLazyByteString)
import qualified Network.WebSockets as WS
import Network.WebSockets.Emulate
import qualified Network.Wai.Handler.WebSockets as WaiWS

responseBS :: ByteString -> Response
responseBS txt = ResponseBuilder statusOK [("Content-Type", "text/plain")] (fromByteString txt)

responseLBS :: L.ByteString -> Response
responseLBS txt = ResponseBuilder statusOK [("Content-Type", "text/plain")] (fromLazyByteString txt)

responseNotFound :: Response
responseNotFound = ResponseBuilder statusNotFound [] (fromByteString "404/resource not found")

type SessionId = Text
type Session = (StreamChan ByteString, StreamChan ByteString)
type SessionMap = Map SessionId Session

type WSApp p = WebSockets p ()
type AppRoute p = [([Text], WSApp p)]

-- | create session if not exists, Left -> old session, Right -> new session.
ensureSession :: MVar SessionMap -> SessionId -> WSApp EmulateProtocol -> IO (Either Session Session)
ensureSession msm sid ws = modifyMVar msm $ \sm ->
    case M.lookup sid sm of
        Just old -> return (sm, Left old)
        Nothing -> do
            new <- (,) <$> newChan <*> newChan
            let sm' = M.insert sid new sm
            _ <- forkIO $ uncurry runEmulator new ws
            return (sm', Right new)

getSession :: MVar SessionMap -> SessionId -> IO (Maybe Session)
getSession msm sid = M.lookup sid <$> readMVar msm

httpApp :: MVar SessionMap -> AppRoute EmulateProtocol -> Application
httpApp msm apps req = case msum $ map match apps of
    Just (app, _:sid:path) -> case path of
        ["xhr"] ->
            liftIO (ensureSession msm sid app) >>=
            either (\(_, outChan) -> do
                      r <- liftIO $ run $ enumChan outChan $$ iterParser (decodeFrame EmulateProtocol)
                      either (error . show) (return . responseLBS . framePayload) r
                   )
                   (\_ -> return $ responseBS "o\n")
        ["xhr_send"] ->
            liftIO (getSession msm sid) >>=
            maybe (return responseNotFound)
                  (\(inChan, _) -> do
                      -- msg <- joinI $ EB.isolate len $$ EL.consume
                      msg <- EL.consume
                      liftIO $ writeChan inChan $ Chunks [toByteString $ encodeFrame EmulateProtocol Nothing $ Frame False BinaryFrame $ L.fromChunks msg]
                      return $ responseBS "" )
        _ -> return responseNotFound
    _ -> return responseNotFound
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
        liftIO $ forM_ clients $ \(_, s) -> sendSink s (textData (msg::L.ByteString))

serverState :: TextProtocol p => ServerState p
serverState = unsafePerformIO $ newMVar M.empty

route :: TextProtocol p => AppRoute p
route = [ ( ["echo"], echo )
       , ( ["broadcast"], broadcast serverState )
       ]

-- newRoute :: IO (forall p. TextProtocol p => AppRoute p)
-- newRoute = do
--     st <- newMVar M.empty
--     return [ ( ["echo"], echo )
--            , ( ["broadcast"], broadcast st )
--            ]

main :: IO ()
main = do
    -- route <- newRoute
    msm <- newMVar M.empty
    runSettings defaultSettings
           { settingsPort = 8080
           , settingsIntercept = WaiWS.intercept (wsApp route)
           } $ httpApp msm route

