module Main where

import Opts
import Run.Accept (runAccept)
import Run.Diff (runDiff)
import Run.Reject (runReject)
import Run.Review (runReview)
import Run.Test (runTest)

main :: IO ()
main = do
  opts <- parseOpts
  print opts
  let Options command = opts
  run command

run :: Command -> IO ()
run = \case
  Test cmd -> runTest cmd
  Diff cmd -> runDiff cmd
  Accept cmd -> runAccept cmd
  Review cmd -> runReview cmd
  Reject cmd -> runReject cmd