module Data.ByteString.Utils where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S

toLazy :: ByteString -> L.ByteString
toLazy = L.fromChunks . (:[])

toStrict :: L.ByteString -> ByteString
toStrict = S.concat . L.toChunks

