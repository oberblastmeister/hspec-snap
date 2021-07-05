module Run.Review (runReview) where

import Control.Monad
import Control.Monad.IO.Class
import Opts (ReviewCmd)
import Path
import Run.Accept (accept)
import Run.Diff (diff)
import Run.Reject (reject)
import System.Console.Haskeline
import qualified Util

runReview :: ReviewCmd -> IO ()
runReview _cmd = do
  snapPairs <- Util.walkDirSnapPairs [reldir|snapshots|]
  runInputT defaultSettings $ forM_ snapPairs \pair -> do
    liftIO $ diff pair
    loop pair

loop :: (Path Abs File, Path Abs File) -> InputT IO ()
loop pair = do
  input <- getInputLine "a: accept, r: reject, s: skip> "
  case input of
    Nothing -> return ()
    Just s -> handleCmd s pair

handleCmd :: String -> (Path Abs File, Path Abs File) -> InputT IO ()
handleCmd cmd pair@(snapPath, newSnapPath) = case cmd of
  "a" -> liftIO $ accept pair
  "r" -> liftIO $ reject newSnapPath
  "s" -> liftIO $ return ()
  _ -> liftIO (putStrLn "invalid command") >> loop pair