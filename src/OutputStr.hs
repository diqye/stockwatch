module OutputStr (
  output,
  outputShow
) where

import System.IO.Unsafe
import Control.Concurrent

 {-# NOINLINE lock #-} 
lock = unsafePerformIO $ newMVar ()

output :: String -> IO ()
output a = do
  takeMVar lock
  putStrLn a
  putMVar lock ()

outputShow :: Show a => a -> IO ()
outputShow a = do
  output $ show a