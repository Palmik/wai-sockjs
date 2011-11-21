{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Sockjs
  ( AppRoute
  , sockjsRoute
  , wsRoute
  , httpRoute
  , receiveMsg
  , sendMsg
  , serverErrorRsp
  ) where

-- imports {{{
import Debug.Trace

import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.List
import Data.Maybe

import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Concurrent

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L

import Data.Enumerator hiding (map, foldl)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as B
import qualified Blaze.ByteString.Builder.Char8 as B

import Data.Attoparsec.Lazy (parse, eitherResult)
import qualified Data.Attoparsec.Enumerator as AE
import Data.Aeson

import Network.HTTP.Types
import Network.Wai

import Network.WebSockets hiding (Request, Response, requestHeaders)
import qualified Network.WebSockets as WS
import Network.WebSockets.Emulate

-- }}}

-- enumerator utils {{{
ioEnum :: IO () -> Enumerator a IO b
ioEnum io step = liftIO io >> E.returnI step

enumChunks :: Monad m => [a] -> Enumerator a m b
enumChunks xs = E.checkContinue0 $ \_ f -> f (E.Chunks xs) >>== E.returnI
-- }}}

-- transport utils {{{

sendMsg :: StreamChan ByteString -> L.ByteString -> IO (Maybe SockjsException)
sendMsg chan = maybe (return $ Just SockjsInvalidJson)
                     (\(SockjsMessage msg) ->
                         ( writeChan chan
                         . Chunks
                         . (:[])
                         . B.toByteString
                         . encodeFrame EmulateProtocol Nothing
                         . Frame True BinaryFrame
                         ) msg >> return Nothing
                     )
             . decode

receiveMsg :: StreamChan ByteString -> IO (Either SockjsException L.ByteString)
receiveMsg chan = do
    stream <- readChan chan
    case stream of
        EOF -> return $ Left SockjsClose
        Chunks xs ->
            either (return . Left . SockjsImpossible . ("internal broken encoding:"++) . show)
                   (return . Right . wrap . encode . SockjsMessage . framePayload)
                   (eitherResult . parse (decodeFrame EmulateProtocol) . L.fromChunks $ xs)
  where
    wrap x = L.concat ["a", x, "\n"]

msgE :: StreamChan ByteString -> Enumerator Builder IO b
msgE chan = checkContinue0 $ \loop f -> do
    mr <- liftIO $ receiveMsg chan
    case mr of
        Left _ -> f $ Chunks [B.fromByteString "c[3000,\"Go away!\"]\n", B.flush]
        Right r -> f (Chunks [B.fromLazyByteString r, B.flush]) >>== loop

-- }}}

-- response utils {{{

response :: L.ByteString -> Response
response s = ResponseBuilder statusOK [("Content-Type", "text/plain")] (B.fromLazyByteString s)

notFoundRsp :: Response
notFoundRsp = ResponseBuilder statusNotFound [] (B.fromByteString "404/resource not found")

notAllowedRsp :: Response
notAllowedRsp = ResponseBuilder statusNotAllowed [] (B.fromByteString "405/method not allowed")

noContentRsp :: Response
noContentRsp = ResponseBuilder statusNoContent [] mempty

serverErrorRsp :: String -> Response
serverErrorRsp msg = ResponseBuilder statusServerError [] (B.fromString msg)

optionsRsp :: Maybe ByteString -> Response
optionsRsp morigin = ResponseBuilder statusNoContent
                    [ ("Cache-Control", "public; max-age=31536000;")
                    , ("Expires", "31536000")
                    , ("Allow", "OPTIONS, POST")
                    , ("access-control-max-age", "31536000")
                    , ("access-control-allow-origin", fromMaybe "*" morigin)
                    , ("access-control-allow-credentials", "true")
                    , ("Set-Cookie", "JSESSIONID=dummy; path=/")
                    ] mempty

-- }}}

-- sessions {{{

type SessionId = Text
type Session = (StreamChan ByteString, StreamChan ByteString)
type SessionMap = Map SessionId Session

type WSApp p = WS.Request -> WebSockets p ()
type AppRoute p = [([Text], WSApp p)]

-- | create session if not exists, Left -> old session, Right -> new session.
ensureSession :: MVar SessionMap -> SessionId -> WSApp EmulateProtocol -> Request -> IO (Either Session Session)
ensureSession msm sid app req = modifyMVar msm $ \sm ->
    case M.lookup sid sm of
        Just old -> return (sm, Left old)
        Nothing -> do
            putStrLn "new session"
            inChan <- newChan
            outChan <- newChan
            let sm' = M.insert sid (inChan, outChan) sm
                req' = RequestHttpPart (rawPathInfo req) (requestHeaders req)
            _ <- forkIO $ runEmulator inChan outChan req' app
            return (sm', Right (inChan, outChan))

getSession :: MVar SessionMap -> SessionId -> IO (Maybe Session)
getSession msm sid = M.lookup sid <$> readMVar msm

-- }}}

-- main sockjs application {{{

sockjsRoute :: MVar SessionMap -> AppRoute EmulateProtocol -> Application
sockjsRoute msm apps req = case (requestMethod req, msum (map match apps)) of
    ("OPTIONS", Just _) -> return $ optionsRsp $ lookup "Origin" (requestHeaders req)

    ("POST", Just (app, path)) -> case path of
        ["chunking_test"] ->
            return $ ResponseEnumerator chunkingTestE

        [_, sid, path'] -> case path' of
            "xhr" ->
                liftIO (ensureSession msm sid app req) >>=
                either (\(_, outChan) -> liftIO $ do
                           r <- either "c[3000,\"Go away!\"]\n" <$> receiveMsg outChan
                           return $ response r
                       )
                       (\(_, outChan) -> liftIO $ do
                           rsp <- readChan outChan
                           print rsp
                           -- TODO parse response
                           return $ response "o\n"
                       )

            "xhr_send" ->
                liftIO (getSession msm sid) >>=
                maybe (return notFoundRsp)
                      (\(inChan, _) -> do
                          msg <- L.fromChunks <$> EL.consume
                          when (L.null msg) $ error "payload expected."
                          liftIO $ sendMsg inChan msg
                          return $ noContentRsp
                        `E.catchError` (\err -> return $ serverErrorRsp $ show err)
                      )

            "xhr_streaming" ->
                return $ ResponseEnumerator $ \f ->
                    ensureSession msm sid app req >>=
                    either (const $ run_ $ liftIO (putStrLn "exists") >> f statusBadRequest [])
                           (\(_, outChan) -> do
                               -- read response
                               rsp <- S.concat . toChunks <$> readChan outChan
                               print rsp
                               -- TODO parse response
                               let iter = f statusOK
                                       [ ("Content-Type", "application/javascript; charset=UTF-8")
                                       , ("Set-Cookie", "JSESSIONID=dummy; path=/")
                                       , ("access-control-allow-origin", "*")
                                       , ("access-control-allow-credentials", "true")
                                       ]
                                   prelude = enumChunks
                                       [ B.fromByteString $ S.replicate 2048 'h'
                                       , B.fromByteString "\n", B.flush
                                       , B.fromByteString "o\n", B.flush
                                       ]
                               run_ $ prelude >==> msgE outChan $$ iter
                           )

            _ -> return notFoundRsp
        _ -> return notFoundRsp
    _ -> return notFoundRsp
  where
    match (prefix, app) = (app, ) <$> stripPrefix prefix (pathInfo req)

    toChunks (Chunks xs) = xs
    toChunks EOF = error "read EOF"

    chunkingTestE :: (Status -> Headers -> Iteratee Builder IO a) -> IO a
    chunkingTestE f = run_ $ combined $$ iter
      where
        iter = f statusOK [ ("Content-Type", "application/javascript; charset=UTF-8")
                          , ("access-control-allow-origin", "*")
                          , ("access-control-allow-credentials", "true")
                          ]
        prelude = enumChunks [ B.fromByteString "h\n", B.flush
                             , B.fromByteString $ S.replicate 2048 ' '
                             , B.fromByteString "h\n", B.flush
                             ]
        step = enumChunks [B.fromByteString "h\n", B.flush]
        combined = foldl (\enum ms -> enum >==> ioEnum (threadDelay $ ms*1000) >==> step)
                         prelude
                         [5, 25, 125, 625, 3125]

-- }}}

wsRoute :: AppRoute Hybi00 -> WSApp Hybi00
wsRoute apps req = case msum $ map match apps of
    Just (app, [_,_,"websocket"]) -> app req
    _ -> WS.rejectRequest req "Forbidden!"
  where path = decodePathSegments $ WS.requestPath req
        match (prefix, app) = (app, ) <$> stripPrefix prefix path

httpRoute :: [([Text], Application)] -> Application -> Application
httpRoute routes fallback req =
    fromMaybe (fallback req) $ msum $ map match routes
  where
    path = pathInfo req
    match (prefix, app) = case stripPrefix prefix path of
        Nothing -> Nothing
        Just rest -> Just $ app req{pathInfo=rest}

