-- | Allows "bundling" or "throttling" of concurrent IO to perform computation
--  only once.
--
-- Useful in situations where concurrency can be expensive, such as file
-- system scans (where concurrent execution usually returns the same
-- result but introduces much disk seeking overhead).
module Control.Concurrent.SharedIO
    ( SharedIO
    , newSharedIO
    , withSharedIO
    ) where

import Control.Applicative
import Control.Concurrent.Chan
import Control.Exception
import Data.IORef


-- | A "sharing lock" on a shared IO computation.
--
-- When it is "free", a computation can be started on it.
--
-- When it is "locked", started computation will not execute,
-- but instead block until the running computation finished, and receive
-- the same result.
newtype SharedIO a = SharedIO (IORef (Maybe (Chan (Either SomeException a))))


-- | Creates a new sharing lock.
newSharedIO :: IO (SharedIO a)
newSharedIO = SharedIO <$> newIORef Nothing


-- | Request to start a computation on the given sharing lock.
--
-- If a computation is already running on that lock, the requested computation
-- will not start, but instead block for the running computation to finish and
-- return the same result for all threads waiting for it.
--
-- Ensures that (a)synchronous exceptions in the computation will be forwarded
-- to all threads waiting for its result.
--
-- Example:
--
--     -- A webserver that lists directory contents, using
--     -- SharedIO to bundle clients fanatically hitting refresh.
--
--     -- Without this, the file seeking would slow the server to a crawl.
--
--     main = do
--         fileScanSharedIO <- newSharedIO
--         runWebserver (handleRequest fileScanSharedIO)
--
--     handleRequest :: SharedIO -> IO [FilePath]
--     handleRequest fileScanSharedIO = do
--         withSharedIO fileScanSharedIO (getDirectoryContents "largefolder")
--
withSharedIO :: SharedIO a -> IO a -> IO a
withSharedIO (SharedIO ref) f = do

    freshChan <- newChan

    m'chan <- atomicModifyIORef' ref $ maybe (Just freshChan, Nothing)         -- use created chan
                                             (\chan -> (Just chan, Just chan)) -- use existing chan

    case m'chan of
        Just chan -> do e'res <- readChan =<< dupChan chan
                        -- Throw ex. in waiting chan if delivering Chan throws.
                        either throwIO return e'res

        Nothing   -> do e'res <- mask $ \restore -> do

                            e'r <- try (restore f)

                            atomicWriteIORef ref Nothing
                            writeChan freshChan e'r

                            return e'r

                        either throwIO return e'res
