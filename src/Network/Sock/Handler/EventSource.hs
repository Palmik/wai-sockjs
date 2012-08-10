{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Network.Sock.Handler.EventSource
( EventSource
) where

------------------------------------------------------------------------------
import           Data.Monoid                      ((<>))
------------------------------------------------------------------------------
import qualified Network.HTTP.Types       as H (status200)
import qualified Network.HTTP.Types.Extra as H
------------------------------------------------------------------------------
import           Network.Sock.Types.Handler
import           Network.Sock.Frame
import           Network.Sock.Session
import           Network.Sock.Request
import           Network.Sock.Handler.Common
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- |
data EventSource = EventSource

-- | EventSource Handler represents the /eventsource route.
--   The /eventsource route serves only to open sessions and to receive stream of incoming messages.
instance Handler EventSource where
    handleReuqest tag req =
        case requestMethod req of
             "GET"     -> do
                 let prelude = yieldAndFlush "\r\n"
                 session <- getSession $ requestSessionID req
                 return $ respondSource tag req H.status200 $ streamingSource tag req 4096 prelude session
             "OPTIONS" -> return $! responseOptions ["OPTIONS", "GET"] req
             _         -> return H.response404

    format _ _ fr = "data: " <> encodeFrame fr <> "\r\n\r\n"

    headers _ req =   H.headerEventStream
                   <> H.headerNotCached
                   <> H.headerCORS "*" req
                   <> H.headerJSESSIONID req