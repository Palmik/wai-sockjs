{-# LANGUAGE OverloadedStrings #-}


module Network.Sock.Handler.HTMLFile
( HTMLFile
) where

------------------------------------------------------------------------------
import qualified Data.Aeson                 as AE
import           Data.Monoid                      ((<>))
import qualified Data.ByteString            as BS (ByteString)
import qualified Data.ByteString.Extra      as BS (convertBS2BL)
import qualified Data.ByteString.Lazy       as BL (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL (replicate)
------------------------------------------------------------------------------
import qualified Network.HTTP.Types          as H (status500, status200)
import qualified Network.HTTP.Types.Extra    as H
------------------------------------------------------------------------------
import           Network.Sock.Types.Handler
import           Network.Sock.Frame
import           Network.Sock.Session
import           Network.Sock.Request
import           Network.Sock.Handler.Common
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- |
data HTMLFile = HTMLFile

-- | HTMLFile Handler represents the /htmlfile route.
--   The /htmlfile route serves only to open sessions and to receive stream of incoming messages.
instance Handler HTMLFile where
    handleReuqest tag req =
        case requestMethod req of
             "GET"     ->
                 case lookup "c" $ requestQuery req of
                      Just (Just c) -> do                          
                          let prelude = yieldAndFlush $ htmlHead c
                          session <- getSession $ requestSessionID req
                          return $ respondSource tag req H.status200 $ streamingSource tag req 4096 prelude session
                      Nothing       -> return $ respondLBS tag req H.status500 "\"callback\" parameter required.\n"
             "OPTIONS" -> return $! responseOptions ["OPTIONS", "GET"] req
             _         -> return H.response404

    format _ _ fr = "<script>\np(" <> AE.encode (encodeFrame fr) <> ");\n</script>\r\n"

    headers _ req =   H.headerHTML
                   <> H.headerNotCached
                   <> H.headerCORS "*" req
                   <> H.headerJSESSIONID req

htmlHead :: BS.ByteString
         -> BL.ByteString
htmlHead callback = 
    "<!doctype html>\n\
    \<html><head>\n\
    \  <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\" />\n\
    \  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />\n\
    \</head><body><h2>Don't panic!</h2>\n\
    \  <script>\n\
    \    document.domain = document.domain;\n\
    \    var c = parent." <> BS.convertBS2BL callback <> ";\n\
    \    c.start();\n\
    \    function p(d) {c.message(d);};\n\
    \    window.onload = function() {c.stop();};\n\
    \  </script>\n" <> BL.replicate 1024 ' '
    