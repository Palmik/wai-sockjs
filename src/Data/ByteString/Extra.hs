module Data.ByteString.Extra
( convertBS2BL
, convertBL2BS
, convertTS2BL
) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL (ByteString, toChunks, fromChunks)
import qualified Data.ByteString      as BS (ByteString, concat)
import qualified Data.Text            as TS (Text)
import qualified Data.Text.Encoding   as TS (encodeUtf8)
------------------------------------------------------------------------------

convertBS2BL :: BS.ByteString -> BL.ByteString
convertBS2BL = BL.fromChunks . (:[])

convertBL2BS :: BL.ByteString -> BS.ByteString
convertBL2BS = BS.concat . BL.toChunks

convertTS2BL :: TS.Text -> BL.ByteString
convertTS2BL = convertBS2BL . TS.encodeUtf8