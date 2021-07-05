module Run.Accept (runAccept, accept) where

import Control.Monad
import Opts (AcceptCmd)
import Path
import Path.IO
import qualified Util

runAccept :: AcceptCmd -> IO ()
runAccept _cmd = do
  snapPairs <- Util.walkDirSnapPairs [reldir|snapshots|]
  forM_ snapPairs accept

accept :: Util.SnapPair -> IO ()
accept (snapPath, newSnapPath) = do
  removeFile snapPath
  renameFile newSnapPath snapPath