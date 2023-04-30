{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Myocardio.ConfigJson
  ( appName,
    mkConfigDir,
    writeDataFile,
    readDataFile,
  )
where

import Control.Applicative (pure)
import Control.Exception (catchJust)
import Control.Monad (guard, (>>=))
import Data.Aeson (FromJSON, eitherDecodeFileStrict, encode)
import Data.Bool (Bool (True))
import Data.ByteString.Lazy (writeFile)
import Data.Either (Either (Left, Right))
import Data.Function ((.))
import Myocardio.ExerciseData (ExerciseData, emptyExerciseData)
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir
  ( getUserConfigDir,
    getUserConfigFile,
  )
import System.IO
  ( FilePath,
    IO,
  )
import System.IO.Error (isDoesNotExistError)
import Prelude (error)

appName :: FilePath
appName = "myocardio"

dataFileName :: IO FilePath
dataFileName = getUserConfigFile appName "data.json"

mkConfigDir :: IO ()
mkConfigDir = getUserConfigDir appName >>= createDirectoryIfMissing True

readJsonFile :: FromJSON a => a -> IO FilePath -> IO a
readJsonFile default_ retrievePath = do
  mkConfigDir
  fn <- retrievePath
  maybeResult <- catchJust (guard . isDoesNotExistError) (eitherDecodeFileStrict fn) (\_ -> pure (Right default_))
  case maybeResult of
    Left e -> error e
    Right v -> pure v

readDataFile :: IO ExerciseData
readDataFile = readJsonFile emptyExerciseData dataFileName

writeDataFile :: ExerciseData -> IO ()
writeDataFile d = do
  mkConfigDir
  fn <- dataFileName
  writeFile fn (encode d)
