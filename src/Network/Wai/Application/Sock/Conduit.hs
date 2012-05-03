{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Wai.Application.Sock.Conduit
( 
) where

------------------------------------------------------------------------------
import           Data.Conduit
------------------------------------------------------------------------------

sourceSock :: (MonadResource m, MonadBaseControl IO m)
           => Source m ByteString
sourceSock = undefined

sinkSock :: (MonadResource m, MonadBaseControl IO m)
         => Sink ByteString m ()
sinkSock = undefined