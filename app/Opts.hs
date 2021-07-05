module Opts
  ( parseOpts,
    Options (..),
    Command (..),
    TestCmd (..),
    DiffCmd (..),
    AcceptCmd (..),
    ReviewCmd (..),
    RejectCmd (..),
  )
where

import Options.Applicative

newtype Options = Options Command
  deriving (Show, Eq)

data Command
  = Test TestCmd
  | Diff DiffCmd
  | Accept AcceptCmd
  | Review ReviewCmd
  | Reject RejectCmd
  deriving (Show, Eq)

data TestCmd = TestCmd
  { delete_unreferenced :: Bool,
    with_cabal :: Bool,
    rest :: [String]
  }
  deriving (Show, Eq)

data DiffCmd = DiffCmd
  deriving (Show, Eq)

data AcceptCmd = AcceptCmd
  deriving (Show, Eq)

data ReviewCmd = ReviewCmd
  deriving (Show, Eq)

data RejectCmd = RejectCmd
  deriving (Show, Eq)

parseOpts :: IO Options
parseOpts =
  customExecParser
    (prefs showHelpOnError)
    ( info (helper <*> opts) (progDesc "a wrapper to run snapshot tests")
    )

opts :: Parser Options
opts =
  Options
    <$> subparser
      ( command
          "test"
          ( info
              (helper <*> (Test <$> test))
              (progDesc "Run all the tests and then review")
          )
          <> command
            "diff"
            ( info
                (helper <*> (Diff <$> diff))
                (progDesc "Show all the snapshot diffs")
            )
          <> command
            "review"
            ( info
                (helper <*> (Review <$> review))
                (progDesc "Interactively review all snapshots")
            )
          <> command
            "accept"
            ( info
                (helper <*> (Accept <$> accept))
                (progDesc "Accept all snapshots")
            )
          <> command
            "reject"
            ( info
                (helper <*> (Reject <$> reject))
                (progDesc "Reject all snapshots")
            )
      )

test :: Parser TestCmd
test =
  TestCmd
    <$> switch
      ( long "delete-unreferenced"
          <> help "delete unreferenced snapshots after running the test"
      )
    <*> switch
      ( long "with-cabal"
          <> short 'c'
          <> help "use cabal to test instead of stack"
      )
    <*> many (argument str (metavar "FORWARDED"))

diff :: Parser DiffCmd
diff =
  pure DiffCmd

review :: Parser ReviewCmd
review =
  pure ReviewCmd

accept :: Parser AcceptCmd
accept =
  pure AcceptCmd

reject :: Parser RejectCmd
reject =
  pure RejectCmd