module Timer where
import Control.Concurrent

type TimerId = ThreadId

setTimeout :: Int -> IO () -> IO TimerId
setTimeout n ac = forkIO $ threadDelay n >> ac

clearTimeout :: TimerId -> IO ()
clearTimeout t = killThread t
