module Debug (dbg, timePure) where

import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hFlush, stdout)

{-# NOINLINE _dbgTimerRef #-}
_dbgTimerRef :: IORef (Maybe UTCTime)
_dbgTimerRef = unsafePerformIO $ newIORef Nothing

-- | Debug timer. Use `liftIO $ dbg "label"` in Servant handlers.
dbg :: String -> IO ()
dbg msg = do
    now <- getCurrentTime
    last <- readIORef _dbgTimerRef
    let delta = case last of
            Nothing -> ""
            Just t  -> let d = diffUTCTime now t
                       in "  [+" ++ show (round (realToFrac d * 1000) :: Int) ++ "ms]"
    putStrLn $ show now ++ " | " ++ msg ++ delta
    hFlush stdout
    writeIORef _dbgTimerRef (Just now)


{-# NOINLINE timePure #-}
timePure :: String -> (() -> a) -> a
timePure label thunk = unsafePerformIO $ do
    t0 <- getCurrentTime
    let res = thunk ()
    t1 <- getCurrentTime
    let ms = round (realToFrac (diffUTCTime t1 t0) * 1000) :: Int
    putStrLn $ show t0 ++ " | " ++ label ++ " took: " ++ show ms ++ "ms"
    hFlush stdout
    return res
