{-# LANGUAGE OverloadedStrings, TupleSections, TemplateHaskell #-}
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
import Control.Monad (forever, msum, forM_)
import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import Blaze.ByteString.Builder (Builder, fromByteString, toByteString, fromLazyByteString)
import Data.Enumerator (run, ($$), Stream(..) )
import qualified Data.Enumerator.List as EL
import Data.Attoparsec.Enumerator (iterParser)

import Network.HTTP.Types (statusOK, statusNotFound, decodePathSegments)
import Network.Wai (Application, Request(Request), Response(..), pathInfo)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, Settings(..))

import Network.WebSockets (WebSockets, Frame(..), FrameType(..), Hybi00, TextProtocol, Protocol)
import Network.WebSockets.Emulate (StreamChan, EmulateProtocol(..), runEmulator, enumChan, encodeFrame, decodeFrame)
import qualified Network.WebSockets as WS

import Data.FileEmbed (embedDir)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static

responseBS :: ByteString -> Response
responseBS txt = ResponseBuilder statusOK [("Content-Type", "text/plain")] (fromByteString txt)

response :: Builder -> Response
response b = ResponseBuilder statusOK [("Content-Type", "text/plain")] b

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
            putStrLn "new session"
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
                      either (error . show) (return . response . fromLazyByteString . framePayload) r
                   )
                   (\_ -> return $ responseBS "o\n")
        ["xhr_send"] ->
            liftIO (getSession msm sid) >>=
            maybe (return responseNotFound)
                  (\(inChan, _) -> do
                      -- msg <- joinI $ EB.isolate len $$ EL.consume
                      msg <- EL.consume
                      liftIO $ print ("post:", msg)
                      liftIO $ writeChan inChan $ Chunks [toByteString $ encodeFrame EmulateProtocol Nothing $ Frame False BinaryFrame $ L.fromChunks msg]
                      return $ responseBS "" )
        _ -> return responseNotFound
    _ -> return responseNotFound
  where
    match (prefix, app) = (app, ) <$> stripPrefix prefix (pathInfo req)

wsApp :: AppRoute Hybi00 -> WS.Request -> WebSockets Hybi00 ()
wsApp apps req = case msum $ map match apps of
    Just (app, [_,_,"websocket"]) -> do
        WS.acceptRequest req
        app
    _ -> WS.rejectRequest req "Forbidden!"

  where path = decodePathSegments $ WS.requestPath req
        match (prefix, app) = (app, ) <$> stripPrefix prefix path

echo :: TextProtocol p => WebSockets p ()
echo = forever $ do
    msg <- WS.receiveData
    WS.sendTextData (msg::L.ByteString)

type ServerState p = Map Text (WS.Sink p)

clientExists :: Protocol p => Text -> ServerState p -> Bool
clientExists name = maybe False (const True) . M.lookup name

chat :: TextProtocol p => MVar (ServerState p) -> WebSockets p ()
chat state = do
    sink <- WS.getSink
    msg <- WS.receiveData
    liftIO $ print msg
    clients <- liftIO $ readMVar state
    case msg of
        _   | not (prefix `T.isPrefixOf` msg) ->
                WS.sendTextData ("Wrong announcement" :: Text)
            | any ($ client)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    WS.sendTextData ("Name cannot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        "cannot be empty" :: Text)
            | clientExists client clients ->
                WS.sendTextData ("User already exists" :: Text)
            | otherwise -> do
                liftIO $ modifyMVar_ state $ \s -> do
                    let s' = M.insert client sink s
                    WS.sendSink sink $ WS.textData $
                        "Welcome! Users: " `mappend`
                        T.intercalate ", " (M.keys s)
                    broadcast (client `mappend` " joined") s'
                    return s'
                talk state client
          where
            prefix = "Hi! I am "
            client = T.drop (T.length prefix) msg

broadcast :: TextProtocol p => Text -> ServerState p -> IO ()
broadcast message clients =
    mapM_ (flip WS.sendSink (WS.textData message)) $ M.elems clients

talk :: TextProtocol p => MVar (ServerState p) -> Text -> WS.WebSockets p ()
talk state user = flip WS.catchWsError catchDisconnect $ do
    msg <- WS.receiveData
    liftIO $ readMVar state >>= broadcast
        (user `mappend` ": " `mappend` msg)
    talk state user
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
            let s' = M.delete user s
            broadcast (user `mappend` " disconnected") s'
            return s'
        _ -> return ()

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
           ]

main :: IO ()
main = do
    port <- read . fromMaybe "3000" . listToMaybe <$> getArgs
    msm <- newMVar M.empty
    runSettings defaultSettings
           { settingsPort = port
           , settingsIntercept = WaiWS.intercept (wsApp wsRoutes)
           } $ httpRoutes [(["static"], staticApp)] (httpApp msm wsRoutes)

