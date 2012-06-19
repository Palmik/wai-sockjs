{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Wai.Sock.Transport
( Transport(..)
, TransportMonad(..)
, Sock(..)
, Application(..)

, module Data.Proxy
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.IO.Class
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy       as LB (ByteString)
import qualified Data.Conduit as C
import           Data.Proxy
------------------------------------------------------------------------------
import qualified Network.Wai as W
------------------------------------------------------------------------------
import           Network.Wai.Sock.Message
import           Network.Wai.Sock.Frame
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Instances

instance Transport tag => Monad (Sock tag) where
    return              = Sock . return
    Sock action >>= f = Sock (action >>= unSock . f)
    

newtype Application = Application { unApplication :: forall tag . Transport tag => Sock tag () }

data Sock tag r where
    Sock :: Transport tag => { unSock :: TransportMonad tag r } -> Sock tag r

type family TransportMonad tag :: * -> *

class ( Functor (TransportMonad tag)
      , Applicative (TransportMonad tag)
      , Monad (TransportMonad tag)
      , MonadIO (TransportMonad tag)
      ) => Transport tag where
    data TransportSession tag :: *

    push :: Proxy tag -> Message -> TransportMonad tag ()
    pull :: Proxy tag -> TransportMonad tag Message

    frameResponse :: Proxy tag -> Frame -> W.Response

    newSession :: Proxy tag -> TransportSession tag
    runSession :: TransportSession tag -> Application -> C.ResourceT IO W.Response