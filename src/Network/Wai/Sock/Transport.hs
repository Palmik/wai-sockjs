{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Sock.Transport
( Transport(..)
, sendFrame
, respondFrame
, respondFrame200
, handleByStatus
) where
    
------------------------------------------------------------------------------
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.MVar.Extra.Lifted
import           Control.Monad.Trans.Control
------------------------------------------------------------------------------
import           Data.Proxy
import qualified Data.Conduit as C
------------------------------------------------------------------------------
import qualified Network.HTTP.Types as H (Status, status200, status204)
import qualified Network.Wai        as W (Request(..), Response(..))
------------------------------------------------------------------------------
import           Network.Wai.Sock.Internal.Types (Transport(..), Session(..), SessionStatus(..))
import           Network.Wai.Sock.Frame
------------------------------------------------------------------------------

sendFrame :: Transport tag
          => Proxy tag
          -> Session
          -> Frame
          -> C.ResourceT IO ()
sendFrame tag ses = send tag ses . format tag

respondFrame :: Transport tag
             => Proxy tag
             -> H.Status
             -> Frame
             -> W.Request
             -> W.Response
respondFrame tag st fr = respond tag st (format tag fr)

respondFrame200 :: Transport tag
                => Proxy tag
                -> Frame
                -> W.Request
                -> W.Response
respondFrame200 tag fr = respond tag H.status200 (format tag fr)

handleByStatus :: (MonadBaseControl IO m, Transport tag)
               => Proxy tag
               -> (Session -> m (SessionStatus, W.Response)) -- ^ SessionFresh handler
               -> (Session -> m (SessionStatus, W.Response)) -- ^ SessionOpened handler
               -> (Session -> m (SessionStatus, W.Response)) -- ^ SessionClosed handler
               -> (Session -> m W.Response) -- ^ Handler for when the session is "Waiting", that is the session status MVar is empty.
               -> Session
               -> m W.Response
handleByStatus tag handleF handleO handleC handleW ses = do
    mvar (handleW ses) -- The MVar is empty, which means there is another connection still open.
         (\s -> case s of
                     SessionFresh   -> handleF ses
                     SessionOpened  -> handleO ses
                     SessionClosed  -> handleC ses
         ) $ sessionStatus ses