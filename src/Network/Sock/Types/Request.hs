{-# LANGUAGE GADTs #-}

module Network.Sock.Types.Request
( Request(..)
) where

------------------------------------------------------------------------------
import qualified Data.Conduit           as C
------------------------------------------------------------------------------
import qualified Network.HTTP.Types.Request as H
------------------------------------------------------------------------------
import           Network.Sock.Types.Session
import           Network.Sock.Types.Application
------------------------------------------------------------------------------

-- | Request wrapper type.
data Request where
    Request ::  H.IsRequest req =>
        { requestRaw :: req
        , requestSessionID :: SessionID
        , requestApplication :: Application (C.ResourceT IO)
        } -> Request