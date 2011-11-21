{-# LANGUAGE OverloadedStrings #-}
module TestEmulate where

import Debug.Trace
import Data.Char
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as M
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import Blaze.ByteString.Builder (fromByteString, toByteString)
import Data.Enumerator (Stream(..))
import Data.Attoparsec (parseOnly)
import Network.WebSockets
import Network.WebSockets.Emulate
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Chan
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

echo :: TextProtocol p => Request -> WebSockets p ()
echo req = do
    acceptRequest req
    forever $ do
        msg <- receiveData
        sendTextData (msg::L.ByteString)

close :: Protocol p => Request -> WebSockets p ()
close req = rejectRequest req "close"

type ServerState p = Map Text (Sink p)

clientExists :: Protocol p => Text -> ServerState p -> Bool
clientExists name = maybe False (const True) . M.lookup name

chat :: TextProtocol p => MVar (ServerState p) -> Request -> WebSockets p ()
chat state req = do
    acceptRequest req
    sink <- getSink
    msg <- receiveData
    clients <- liftIO $ readMVar state
    case msg of
        _   | not (prefix `T.isPrefixOf` msg) ->
                sendTextData ("[\"Wrong announcement\"]" :: Text)
            | any ($ client)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    sendTextData ("[\"Name cannot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        "cannot be empty\"]" :: Text)
            | clientExists client clients ->
                sendTextData ("User already exists" :: Text)
            | otherwise -> do
                liftIO $ modifyMVar_ state $ \s -> do
                    let s' = M.insert client sink s
                    sendSink sink $ textData $
                        "Welcome! Users: " `mappend`
                        T.intercalate ", " (M.keys s)
                    broadcast (client `mappend` " joined") s'
                    return s'
                talk state client
          where
            prefix = "[\"Hi! I am "
            client = T.drop (T.length prefix) msg

broadcast :: TextProtocol p => Text -> ServerState p -> IO ()
broadcast message clients =
    mapM_ (flip sendSink (textData message)) $ M.elems clients

talk :: TextProtocol p => MVar (ServerState p) -> Text -> WebSockets p ()
talk state user = flip catchWsError catchDisconnect $ do
    msg <- receiveData
    liftIO $ readMVar state >>= broadcast
        (user `mappend` ": " `mappend` msg)
    talk state user
  where
    catchDisconnect e = case fromException e of
        Just ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
            let s' = M.delete user s
            broadcast (user `mappend` " disconnected") s'
            return s'
        _ -> return ()

writeMsg :: StreamChan ByteString -> L.ByteString -> IO ()
writeMsg chan msg = writeChan chan $ Chunks [toByteString $ encodeFrame EmulateProtocol Nothing $ Frame True BinaryFrame $ fromLazyByteString msg]

readMsg :: StreamChan ByteString -> IO (Maybe ByteString)
readMsg chan = do
    stream <- readChan chan
    case stream of
        EOF -> trace "msg EOF" $ return Nothing
        Chunks xs -> return $ Just $ (\xs -> S.concat $ ["a"]++xs++["\n"]) $ L.toChunks $ framePayload $ either (error . show) id $ parseOnly (decodeFrame EmulateProtocol) $ S.concat xs

runApp :: (Request -> WebSockets EmulateProtocol ()) -> IO (StreamChan ByteString, StreamChan ByteString)
runApp app = do
    inChan <- newChan
    outChan <- newChan
    let req = RequestHttpPart "/" [] 
    forkIO $ runEmulator inChan outChan req app
    return (inChan, outChan)

tests :: [Test]
tests = [ testGroup "simple"
            [ testCase "echo" testEcho
            , testCase "chat" testChat
            ]
        ]

testEcho :: Assertion
testEcho = do
    (inChan, outChan) <- runApp echo
    readChan outChan-- >>= print
    writeMsg inChan "xx"
    readMsg outChan >>= (@?= Just "xx")

testChat :: Assertion
testChat = do
    msm <- newMVar M.empty

    (inChan1, outChan1) <- runApp (chat msm)
    readChan outChan1 >>= print
    writeMsg inChan1 "Hi! I am test1"

    (inChan2, outChan2) <- runApp (chat msm)
    readChan outChan2 >>= print
    writeMsg inChan2 "Hi! I am test2"

    readMsg outChan1 >>= (@?= Just "Welcome! Users: ")
    readMsg outChan1 >>= (@?= Just "test1 joined")
    readMsg outChan2 >>= (@?= Just "Welcome! Users: test1")
    readMsg outChan2 >>= (@?= Just "test2 joined")
    readMsg outChan1 >>= (@?= Just "test2 joined")

main = defaultMain tests
