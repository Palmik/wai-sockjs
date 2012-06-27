{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Sock.Transport
( Transport(..)
, sendFrame
, frameResponse
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
import qualified Network.Wai as W (Request(..), Response(..))
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

frameResponse :: Proxy tag
              -> W.Request
              -> Frame
              -> W.Response
frameResponse tag = undefined

handleByStatus :: (MonadBaseControl IO m, Transport tag)
               => Proxy tag
               -> (Session -> m (SessionStatus, W.Response)) -- ^ SessionFresh handler
               -> (Session -> m (SessionStatus, W.Response)) -- ^ SessionOpened handler
               -> (Session -> m (SessionStatus, W.Response)) -- ^ SessionClosed handler
               -> (Session -> m W.Response) -- ^ Handler for when the session is "Waiting", that is the session status MVar is empty.
               -> W.Request
               -> Session
               -> m W.Response
handleByStatus tag handleF handleO handleC handleW req ses = do
    mvar (handleW ses) -- The MVar is empty, which means there is another connection still open.
         (\s -> case s of
                     SessionFresh   -> handleF ses
                     SessionOpened  -> handleO ses
                     SessionClosed  -> handleC ses
         ) $ sessionStatus ses