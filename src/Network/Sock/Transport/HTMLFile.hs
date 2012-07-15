{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Network.Sock.Transport.HTMLFile
( HTMLFile
) where

------------------------------------------------------------------------------
import           Control.Monad.Base                          (MonadBase, liftBase)
import qualified Control.Monad.STM                    as STM (STM, atomically)
import           Control.Monad.Trans.Class                   (lift)
------------------------------------------------------------------------------
import qualified Data.Aeson                 as AE
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.ByteString.Extra
import qualified Data.Conduit               as C
import           Data.Monoid                      ((<>))
------------------------------------------------------------------------------
import qualified Network.HTTP.Types          as H (status200, status500)
import qualified Network.HTTP.Types.Response as H
import qualified Network.HTTP.Types.Extra    as H
------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder as B
------------------------------------------------------------------------------
import           Network.Sock.Types.Transport
import           Network.Sock.Application
import           Network.Sock.Frame
import           Network.Sock.Session
import           Network.Sock.Server
import           Network.Sock.Request
import           Network.Sock.Transport.Streaming
import           Network.Sock.Transport.Utility
------------------------------------------------------------------------------

atomically :: MonadBase IO m => STM.STM a -> m a
atomically = liftBase . STM.atomically

------------------------------------------------------------------------------
-- |
data HTMLFile = HTMLFile

-- | HTMLFile Transport represents the /htmlfile route.
--   The /htmlfile route serves only to open sessions and to receive stream of incoming messages.
instance Transport HTMLFile where
    handleIncoming tag req =
        case requestMethod req of
             "GET"     -> getSession sid >>= handleByStatus tag handleF handleO handleC handleW
             "OPTIONS" -> return $! responseOptions ["OPTIONS", "GET"] req
             _         -> return H.response404

        where
            handleF :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleF ses =
                -- TODO: Start the timers.
                case lookup "c" $ requestQuery req of
                     Just (Just c) -> do
                         lift $ forkApplication app ses
                         return (SessionOpened, respondSource tag req H.status200 (source c))
                     _             -> return $ (SessionOpened, respondLBS tag req H.status500 "\"callback\" parameter required.\n")
                where source c = do
                          C.yield $ C.Chunk $ htmlHead c
                          C.yield C.Flush
                          C.yield $ C.Chunk $ B.fromLazyByteString $ format tag req FrameOpen 
                          C.yield C.Flush
                          streamSource ses

            handleO :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleO = handleF

            handleC :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleC _ = return (SessionClosed, respondFrame200 tag req $ FrameClose 3000 "Go away!")

            handleW :: H.IsResponse res => Session -> Server res
            handleW _ = return $ respondFrame200 tag req $ FrameClose 2010 "Another connection still open"

            streamSource :: Session -> C.Source (C.ResourceT IO) (C.Flush B.Builder)
            streamSource ses = streamingSource tag 4096 ses req

            sid = requestSessionID req
            app = requestApplication req

    format _ _ fr = "<script>\np(" <> AE.encode (encodeFrame fr) <> ");\n</script>\r\n"

    headers _ req =   H.headerHTML
                   <> H.headerNotCached
                   <> H.headerCORS "*" req
                   <> H.headerJSESSIONID req

htmlHead :: BS.ByteString
         -> B.Builder
htmlHead callback = B.fromLazyByteString $ 
    "<!doctype html>\n\
    \<html><head>\n\
    \  <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\" />\n\
    \  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />\n\
    \</head><body><h2>Don't panic!</h2>\n\
    \  <script>\n\
    \    document.domain = document.domain;\n\
    \    var c = parent." <> convertBS2BL callback <> ";\n\
    \    c.start();\n\
    \    function p(d) {c.message(d);};\n\
    \    window.onload = function() {c.stop();};\n\
    \  </script>\n" <> BL.replicate 1024 ' '
                                                                                                     