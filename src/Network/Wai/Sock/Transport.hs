{-# LANGUAGE FlexibleContexts #-}

module Network.Wai.Sock.Transport
( Transport(..)
, sendFrame
, frameResponse
) where
    
------------------------------------------------------------------------------
import           Control.Monad.Trans.Control
------------------------------------------------------------------------------
import           Data.Proxy
------------------------------------------------------------------------------
import           Network.Wai.Sock.Internal.Types (Transport(..))
import           Network.Wai.Sock.Frame
------------------------------------------------------------------------------
import qualified Network.Wai        as W (Request(..), Response(..))
------------------------------------------------------------------------------

sendFrame :: (MonadBaseControl IO m, Transport tag)
          => Proxy tag
          -> Frame
          -> m ()
sendFrame tag = send tag . format tag

frameResponse :: Proxy tag
              -> W.Request
              -> Frame
              -> W.Response
frameResponse tag = undefined