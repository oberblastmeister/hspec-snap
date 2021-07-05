module Run.Reject (runReject, reject) where

import Control.Monad
import Opts (RejectCmd)
import Path
import Path.IO
import qualified Util

runReject :: RejectCmd -> IO ()
runReject _cmd = do
  snapPairs <- Util.walkDirSnapPairs [reldir|snapshots|]
  forM_ snapPairs \(_, newSnapPath) -> do
    newSnapExists <- doesFileExist newSnapPath
    when newSnapExists do
      reject newSnapPath

reject :: Path Abs File -> IO ()
reject = removeFile