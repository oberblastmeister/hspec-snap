module Run.Diff (runDiff, diff) where

import Control.Exception
import Control.Monad
import Opts (DiffCmd)
import Path
import System.Exit (ExitCode (..))
import System.Process.Typed
import qualified Util

runDiff :: DiffCmd -> IO ()
runDiff _diff = do
  let snapDir = [reldir|snapshots|]
  snapFiles <- Util.walkDirSnapPairs snapDir
  forM_ snapFiles diff

diff :: Util.SnapPair -> IO ()
diff = uncurry runGitDiff

runGitDiff :: Path b t -> Path b' t' -> IO ()
runGitDiff p p' = do
  let gitConfig = setStderr inherit $ setStdout inherit $ gitDiffProc p p'
  code <- runProcess gitConfig
  case code of
    ExitSuccess -> return ()
    (ExitFailure 1) -> return ()
    _ -> throwIO $ userError "git diff did not return 0 or 1"

gitDiffProc :: Path b t -> Path b' t' -> ProcessConfig () () ()
gitDiffProc p p' =
  proc "git" ["--no-pager", "diff", "--no-index", toFilePath p, toFilePath p']