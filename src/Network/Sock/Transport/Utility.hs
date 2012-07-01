{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Sock.Transport.Utility
( respondFrame
, respondFrame200
, handleByStatus

, responseOptions
) where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar.Extra.Lifted
------------------------------------------------------------------------------
import qualified Data.ByteString as BS
import           Data.Proxy
------------------------------------------------------------------------------
import qualified Network.HTTP.Types as H (Status, status200)
import qualified Network.HTTP.Types.Request  as H
import qualified Network.HTTP.Types.Response as H
import qualified Network.HTTP.Types.Extra    as H
------------------------------------------------------------------------------
import           Network.Sock.Types.Session
import           Network.Sock.Types.Transport
import           Network.Sock.Types.Frame
import           Network.Sock.Types.Server
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Used as a response to http://example.com/<application_prefix>/<server_id>/<session_id>/<transport>
--
--   Documentation: http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-7
responseOptions :: [BS.ByteString]
                -> H.Request
                -> H.Response
responseOptions methods req = H.response204 headers ""
    where headers =    [("Access-Control-Allow-Methods", BS.intercalate ", " methods)]
                    ++ H.headerCached
                    ++ H.headerCORS "*" req


respondFrame :: Transport tag
             => Proxy tag
             -> H.Status
             -> Frame
             -> Request
             -> H.Response
respondFrame tag st fr = respond tag st (format tag fr)

respondFrame200 :: Transport tag
                => Proxy tag
                -> Frame
                -> Request
                -> H.Response
respondFrame200 tag fr = respond tag H.status200 (format tag fr)

handleByStatus :: Transport tag
               => Proxy tag
               -> (Session -> Server (SessionStatus, H.Response)) -- ^ SessionFresh handler
               -> (Session -> Server (SessionStatus, H.Response)) -- ^ SessionOpened handler
               -> (Session -> Server (SessionStatus, H.Response)) -- ^ SessionClosed handler
               -> (Session -> Server H.Response) -- ^ Handler for when the session is "Waiting", that is the session status MVar is empty.
               -> Session
               -> Server H.Response
handleByStatus tag handleF handleO handleC handleW ses =
    mvar (handleW ses) -- The MVar is empty, which means there is another connection still open.
         (\s -> case s of
                     SessionFresh   -> handleF ses
                     SessionOpened  -> handleO ses
                     SessionClosed  -> handleC ses
         ) $ sessionStatus ses