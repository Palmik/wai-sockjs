module Data.Enumerator.Utils where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.Enumerator
import qualified Data.Enumerator.List as EL
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as B

import Control.Concurrent.STM
import Network.WebSockets.Lite.Emulate

-- | An `Enumerator' only do IO.
ioEnum :: IO () -> Enumerator a IO b
ioEnum io step = liftIO io >> returnI step

enumSingle :: Monad m => a -> Enumerator a m b
enumSingle = enumChunks . (:[])

enumChunks :: Monad m => [a] -> Enumerator a m b
enumChunks xs = checkContinue0 $ \_ f -> f (Chunks xs) >>== returnI

-- | set a limit to the stream size, but don't break chunk.
limit :: Monad m => Int64 -> Enumeratee ByteString ByteString m b
limit n step | n <= 0 = return step
limit n (Continue k) = continue loop where
    loop (Chunks []) = continue loop
    loop (Chunks xs) = iter where
        len = L.length (L.fromChunks xs)
        iter = if len <= n
            then k (Chunks xs) >>== limit (n - len)
            else k (Chunks xs) >>== (`yield` Chunks [])
    loop EOF = k EOF >>== (`yield` EOF)
limit _ step = return step

-- | fetch multiple (at least one) items from TChan at a time, if TChan is empty, block on it.
readTChan' :: TChan a -> STM [a]
readTChan' ch = (:) <$> readTChan ch <*> readRest ch
  where
    readRest ch = do
        empty <- isEmptyTChan ch
        if empty
          then return []
          else (:) <$> readTChan ch <*> readRest ch

-- | fetch multiple (at least one) chunks from TChan at a time, and combine them into one.
enumStreamChanContents :: StreamChan a -> Enumerator [a] IO b
enumStreamChanContents ch = checkContinue0 $ \loop f -> do
    streams <- liftIO $ atomically $ readTChan' ch
    let chunks = takeWhile isChunk streams
        datas = concat [xs | (Chunks xs) <- chunks]
    if null chunks
      then f EOF >>== returnI
      else f (Chunks [datas]) >>== loop
  where isChunk (Chunks _) = True
        isChunk _          = False

-- | like `Enumerator.List.concatMap' , but terminate when return Nothing
concatMapMaybe :: Monad m => (ao -> Maybe [ai])
           -> Enumeratee ao ai m b
concatMapMaybe f = checkDone (continue . step) where
    step k EOF = yield (Continue k) EOF
    step k (Chunks xs) = loop k xs
    
    loop k [] = continue (step k)
    loop k (x:xs) = do
        case f x of
            Nothing -> k EOF >>==
                (`yield` (Chunks xs))
            Just fx -> k (Chunks fx) >>==
                checkDoneEx (Chunks xs) (`loop` xs)

chunking :: Monad m => Enumeratee ByteString Builder m a
chunking = EL.concatMap ((:[B.flush]) . B.fromByteString)
