{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Attoparsec.Char8 as A
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import Blaze.ByteString.Builder (Builder, fromByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S

import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class

import Network.Server.ScalableServer
import Chat hiding (main)

type Request = (ByteString, Text, Text)

parseRequest :: A.Parser Request
parseRequest = (,,) <$> A.takeTill A.isSpace <* A.skipWhile (==' ')
                    <*> (T.decodeUtf8 <$> A.takeTill A.isSpace)
                        <* A.skipWhile (==' ')
                    <*> (T.decodeUtf8 <$> A.takeTill (A.inClass "\r\n"))
                        <* A.string "\r\n"

authenticated :: Request -> Client -> ServerM Text
authenticated (cmd, identity, msg) client  = case cmd of
    "broadcast" -> broadcast msg >> return "success.\n"
    "recv" -> recv client
    "echo" -> echo msg client >> return "success.\n"

handle :: Request -> ServerM Text
handle ("join", identity, _) = addClient identity >> return "success.\n"
handle req@(_, identity, _) = do
    mc <- getClient identity
    case mc of
        Nothing -> return "not exist.\n"
        Just c -> authenticated req c

handler :: TServerState -> Request -> IO Builder
handler st req = fromByteString . T.encodeUtf8 <$> runServerM (handle req) st

main = do
    st <- newServerState
    let definition = RequestPipeline parseRequest (handler st) 10
    _ <- forkIO $ runServer definition 6004
    forever $ threadDelay (1000000 * 60)
