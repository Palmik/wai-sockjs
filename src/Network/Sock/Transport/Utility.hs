{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Sock.Transport.Utility
( respondFrame
, respondFrame200
, respondLBS
, respondSource
, handleByStatus

, responseOptions
) where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar.Extra.Lifted
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
import qualified Data.Conduit         as C
import           Data.Proxy
------------------------------------------------------------------------------
import qualified Network.HTTP.Types          as H (Status, status200)
import qualified Network.HTTP.Types.Request  as H
import qualified Network.HTTP.Types.Response as H
import qualified Network.HTTP.Types.Extra    as H
------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder as B
------------------------------------------------------------------------------
import           Network.Sock.Frame
import           Network.Sock.Request
import           Network.Sock.Server
import           Network.Sock.Session
import           Network.Sock.Types.Transport
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Used as a response to http://example.com/<application_prefix>/<server_id>/<session_id>/<transport>
--
--   Documentation: http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-7
responseOptions :: (H.IsResponse res, H.IsRequest req)
                => [BS.ByteString]
                -> req
                -> res
responseOptions methods req = H.response204 headers ""
    where headers =    [("Access-Control-Allow-Methods", BS.intercalate ", " methods)]
                    ++ H.headerCached
                    ++ H.headerCORS "*" req


respondFrame :: (H.IsResponse res, Transport tag)
             => Proxy tag
             -> Request
             -> H.Status
             -> Frame
             -> res
respondFrame tag req st fr = respondLBS tag req st (format tag req fr)

respondFrame200 :: (H.IsResponse res, Transport tag)
                => Proxy tag
                -> Request
                -> Frame                
                -> res
respondFrame200 tag req = respondFrame tag req H.status200

respondLBS :: (H.IsResponse res, Transport tag)
           => Proxy tag
           -> Request
           -> H.Status
           -> BL.ByteString           
           -> res
respondLBS tag req status body = H.responseLBS status (headers tag req) body

respondSource :: (H.IsResponse res, Transport tag)
              => Proxy tag
              -> Request
              -> H.Status
              -> (C.Source (C.ResourceT IO) (C.Flush B.Builder))              
              -> res
respondSource tag req status source = H.responseSource status (headers tag req) source

handleByStatus :: (H.IsResponse res, Transport tag)
               => Proxy tag
               -> (Session -> Server (SessionStatus, res)) -- ^ SessionFresh handler
               -> (Session -> Server (SessionStatus, res)) -- ^ SessionOpened handler
               -> (Session -> Server (SessionStatus, res)) -- ^ SessionClosed handler
               -> (Session -> Server res) -- ^ Handler for when the session is "Waiting", that is the session status MVar is empty.
               -> Session
               -> Server res
handleByStatus tag handleF handleO handleC handleW ses =
    mvar (handleW ses) -- The MVar is empty, which means there is another connection still open.
         (\s -> case s of
                     SessionFresh   -> handleF ses
                     SessionOpened  -> handleO ses
                     SessionClosed  -> handleC ses
         ) $ sessionStatus ses