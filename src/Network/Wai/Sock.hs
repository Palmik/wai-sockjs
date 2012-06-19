{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Sock
( S.Sock
, S.Application(S.Application)
, push
, pull
, S.runSockServer
, S.sock
) where

import           Data.Proxy
import qualified Network.Wai.Sock.Transport   as S
import qualified Network.Wai.Sock.Message     as S
import qualified Network.Wai.Sock.Handler     as S

push :: forall tag . S.Transport tag => S.Message -> S.Sock tag ()
push = S.Sock . S.push (Proxy :: Proxy tag)

pull :: forall tag . S.Transport tag => S.Sock tag S.Message
pull = S.Sock $ S.pull (Proxy :: Proxy tag)