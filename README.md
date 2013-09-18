haskell-sharedio
================

Allows "bundling" or "throttling" of concurrent IO to perform computation
only once.

Useful in situations where concurrency can be expensive, such as file
system scans (where concurrent execution usually returns the same
result but introduces much disk seeking overhead).


Example
-------

```haskell
import Control.Concurrent.SharedIO (SharedIO, newSharedIO, withSharedIO)

-- A webserver that lists directory contents, using
-- SharedIO to bundle clients fanatically hitting refresh.

-- Without this, the file seeking would slow the server to a crawl.

main = do
    fileScanSharedIO <- newSharedIO
    runWebserver (handleRequest fileScanSharedIO)

handleRequest :: SharedIO -> IO [FilePath]
handleRequest fileScanSharedIO = do
    withSharedIO fileScanSharedIO (getDirectoryContents "largefolder")
```
