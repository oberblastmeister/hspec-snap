module Util
  ( walkDirSnapFiles,
    walkDirSnapPairs,
    makeNewSnapPath,
    SnapPair,
  )
where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe
import Path
import Path.IO

type SnapPair = (Path Abs File, Path Abs File)

walkHandler ::
  (MonadIO m, MonadThrow m) =>
  Path Abs Dir ->
  [Path Abs Dir] ->
  [Path Abs File] ->
  m [Path Abs File]
walkHandler _dir _subDirs files = do
  filterM
    ( \file ->
        do
          ext <- fileExtension file
          return $ ext == ".snap"
    )
    files

walkDirSnapPairs ::
  (MonadIO m, MonadThrow m) =>
  Path b Dir ->
  m [(Path Abs File, Path Abs File)]
walkDirSnapPairs dir = do
  snapPaths <- walkDirSnapFiles dir
  mapM
    ( \snapPath -> do
        let newSnapPath = Util.makeNewSnapPath snapPath
        return (snapPath, newSnapPath)
    )
    snapPaths
    >>= filterM
      (\(_, newSnapPath) -> doesFileExist newSnapPath)

walkDirSnapFiles :: (MonadIO m, MonadThrow m) => Path b Dir -> m [Path Abs File]
walkDirSnapFiles = walkDirAccum Nothing walkHandler

makeNewSnapPath :: Path b File -> Path b File
makeNewSnapPath p = fromJust $ addExtension ".new" p