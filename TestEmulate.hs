{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as M
import Network.WebSockets
import Network.WebSockets.Emulate
import Data.ByteString (ByteString)
import Control.Concurrent
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Sockjs
import Apps

runApp :: (Request -> WebSockets EmulateProtocol ()) -> IO (StreamChan ByteString, StreamChan ByteString)
runApp app = do
    inChan <- newChan
    outChan <- newChan
    let req = RequestHttpPart "/" [] 
    _ <- forkIO $ runEmulator inChan outChan req app
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
    readChan outChan >>= print
    sendMsg inChan "[\"xx\"]"
    receiveMsg outChan >>= (@?= Just "a[\"xx\"]\n")

testChat :: Assertion
testChat = do
    msm <- newMVar M.empty

    (inChan1, outChan1) <- runApp (chat msm)
    readChan outChan1 >>= print
    sendMsg inChan1 "[\"join\", \"test1\"]"

    (inChan2, outChan2) <- runApp (chat msm)
    readChan outChan2 >>= print
    sendMsg inChan2 "[\"join\", \"test2\"]"

    receiveMsg outChan1 >>= (@?= Just "a[\"Welcome! Users: \"]\n")
    receiveMsg outChan1 >>= (@?= Just "a[\"test1 joined\"]\n")
    receiveMsg outChan2 >>= (@?= Just "a[\"Welcome! Users: test1\"]\n")
    receiveMsg outChan2 >>= (@?= Just "a[\"test2 joined\"]\n")
    receiveMsg outChan1 >>= (@?= Just "a[\"test2 joined\"]\n")

main :: IO ()
main = defaultMain tests
