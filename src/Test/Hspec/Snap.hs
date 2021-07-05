module Test.Hspec.Snap where

import Control.Monad.Except
import Control.Monad.IO.Class ()
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Path
import Path.IO
import Test.Hspec
import Test.Hspec.Core.Spec
import Prelude hiding (readFile, writeFile)

data SnapshotSettings = SnapshotSettings
  { snapshotDir :: Path Rel Dir,
    snapshotName :: Path Rel File
  }

defaultSettings :: Path Rel File -> SnapshotSettings
defaultSettings snapshotName = SnapshotSettings {snapshotDir = [reldir|snapshots|], snapshotName}

snapshotPath :: SnapshotSettings -> Path Rel File
snapshotPath settings = do
  let path = snapshotDir settings </> snapshotName settings
  fromMaybe (error $ "failed to add .snap extension to path: " ++ show path) $ addExtension ".snap" path

newSnapshotPath :: SnapshotSettings -> Path Rel File
newSnapshotPath settings = do
  let path = snapshotPath settings
  -- let res = addExtension "new" path
  fromMaybe (error $ "failed to add .new extension to path: " ++ show path) $ addExtension ".new" path

data SnapshotSpec = SnapshotSpec
  { settings :: SnapshotSettings,
    contents :: Text
  }

defaultSnapshot :: String -> Text -> SnapshotSpec
defaultSnapshot name contents =
  SnapshotSpec
    { settings =
        defaultSettings $
          fromMaybe (error "invaild path") (parseRelFile name),
      contents
    }

instance Example SnapshotSpec where
  type Arg SnapshotSpec = ()

  evaluateExample e = evaluateExample (\() -> e)

instance Example (arg -> SnapshotSpec) where
  type Arg (arg -> SnapshotSpec) = arg

  evaluateExample snapshot _ action _ = do
    ref <- newIORef (Result "" Success)
    action $ \arg -> do
      r <- runSnap (snapshot arg)
      writeIORef ref r
    readIORef ref

data SnapshotError
  = MismatchedOutput
  | MissingSnapFile

errorToHspecResult :: SnapshotError -> Result
errorToHspecResult = \case
  MismatchedOutput ->
    Result "Mismatched output" $
      Failure Nothing (Reason "The new output did not match the existing snap file")
  MissingSnapFile ->
    Result "Missing snap file" $
      Failure
        Nothing
        ( Reason
            "There was a new snap file but the existing one was not found"
        )

data SnapshotSuccess
  = SameOutput
  | StoredNewSnapshot
  | FirstExecutionSucceed

successToHspecResult :: SnapshotSuccess -> Result
successToHspecResult = \case
  SameOutput -> Result "Same output" Success
  StoredNewSnapshot -> Result "A new snapshot was stored" Success
  FirstExecutionSucceed -> Result "The first executation succeeded" Success

readFile :: MonadIO m => Path b t -> m Text
readFile p = liftIO $ TIO.readFile (toFilePath p)

writeFile :: MonadIO m => Path b t -> Text -> m ()
writeFile p t = liftIO $ TIO.writeFile (toFilePath p) t

runSnap :: SnapshotSpec -> IO Result
runSnap snapshot = do
  res <- runExceptT (evaluateSnap snapshot)
  return $ either errorToHspecResult successToHspecResult res

evaluateSnap :: (MonadIO m, MonadError SnapshotError m) => SnapshotSpec -> m SnapshotSuccess
evaluateSnap snapshot = do
  let conf = settings snapshot
  let cont = contents snapshot
  let snapDir = snapshotDir conf
  createDirIfMissing False snapDir
  let snapPath = snapshotPath conf
  let snapNewPath = newSnapshotPath conf
  existsSnapPath <- doesFileExist snapPath
  existsSnapNewPath <- doesFileExist snapNewPath

  case (existsSnapNewPath, existsSnapPath) of
    (False, False) -> do
      writeFile snapPath cont
      return FirstExecutionSucceed
    (True, True) -> do
      actual <- readFile snapPath
      if cont /= actual
        then do
          writeFile snapNewPath cont
          throwError MismatchedOutput
        else return SameOutput
    (True, False) -> throwError MissingSnapFile
    (False, True) -> do
      actual <- readFile snapPath
      if contents snapshot /= actual
        then do
          writeFile snapNewPath cont
          throwError MismatchedOutput
        else return SameOutput

snap :: FilePath -> String -> SpecWith ()
snap s showable =
  it ("should work for snapshot path: " ++ s) $
    defaultSnapshot
      s
      (T.pack showable)