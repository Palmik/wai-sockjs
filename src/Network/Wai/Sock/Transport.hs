{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Sock.Transport
( Transport(..)
, sendFrame
, respondFrame
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
import qualified Network.Wai        as W (Request(..), Response(..))
import           Network.Wai.Extra
------------------------------------------------------------------------------
import           Network.Wai.Sock.Internal.Types (Transport(..), Session(..), SessionStatus(..), Request(..))
import           Network.Wai.Sock.Frame
import           Network.Wai.Sock.Server
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Used as a response to http://example.com/<application_prefix>/<server_id>/<session_id>/<transport>
--
--   Documentation: http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-7
responseOptions :: [BS.ByteString]
                -> W.Request
                -> W.Response
responseOptions methods req = response204 headers ""
    where headers =    [("Access-Control-Allow-Methods", BS.intercalate ", " methods)]
                    ++ headerCached
                    ++ headerCORS "*" req


{-
HTTP/1.1 204 No Content
Access-Control-Allow-Origin: *
Access-Control-Allow-Credentials: true
Cache-Control: public, max-age=31536000
Expires: Mon, 01 Jul 2013 12:07:17 GMT
Access-Control-Allow-Methods: OPTIONS, GET
Access-Control-Max-Age: 31536000
Date: Sun, 01 Jul 2012 12:07:17 GMT
Connection: keep-alive
-}

sendFrame :: Transport tag
          => Proxy tag
          -> Session
          -> Frame
          -> Server ()
sendFrame tag ses = send tag ses . format tag

respondFrame :: Transport tag
             => Proxy tag
             -> H.Status
             -> Frame
             -> Request
             -> W.Response
respondFrame tag st fr = respond tag st (format tag fr)

respondFrame200 :: Transport tag
                => Proxy tag
                -> Frame
                -> Request
                -> W.Response
respondFrame200 tag fr = respond tag H.status200 (format tag fr)

handleByStatus :: Transport tag
               => Proxy tag
               -> (Session -> Server (SessionStatus, W.Response)) -- ^ SessionFresh handler
               -> (Session -> Server (SessionStatus, W.Response)) -- ^ SessionOpened handler
               -> (Session -> Server (SessionStatus, W.Response)) -- ^ SessionClosed handler
               -> (Session -> Server W.Response) -- ^ Handler for when the session is "Waiting", that is the session status MVar is empty.
               -> Session
               -> Server W.Response
handleByStatus tag handleF handleO handleC handleW ses =
    mvar (handleW ses) -- The MVar is empty, which means there is another connection still open.
         (\s -> case s of
                     SessionFresh   -> handleF ses
                     SessionOpened  -> handleO ses
                     SessionClosed  -> handleC ses
         ) $ sessionStatus ses