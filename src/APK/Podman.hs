{-# LANGUAGE CPP #-}

module APK.Podman
  ( Config(..)
  , HostPaths(..)
  , PackageName
  , build
  , getHostPaths
  , index
  , run
  , update
  )
where

import APK.Prelude
import Control.Monad (unless)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Exit (ExitCode(ExitSuccess))
import System.Path ((</>))

import qualified APK.TH                as TH
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.List             as List
import qualified Network.AWS           as AWS
import qualified Network.AWS.Data.Body as AWS
import qualified System.Path           as Path
import qualified System.Path.Directory as Path
import qualified System.Process.Typed  as Process

type ContainerName = BoundText "ContainerName"
type ImageName     = BoundText "ImageName"
type PackageName   = BoundText "PackageName"

data Config = Config
  { arguments      :: [String]
  , command        :: String
  , repositoryPath :: Path.RelDir
  , workPath       :: Path.AbsDir
  }

data HostPaths = HostPaths
  { hostAbuildPath             :: Path.AbsDir
  , hostCachePath              :: Path.AbsDir
  , hostHomePath               :: Path.AbsDir
  , hostPackagesPath           :: Path.AbsDir
  , hostRepositoryPackagesPath :: Path.AbsDir
  , hostRepositoryPath         :: Path.AbsDir
  , hostWorkingPath            :: Path.AbsDir
  }

defaultConfig :: Config
defaultConfig = Config
  { arguments      = []
  , command        = "exit"
  , repositoryPath = Path.relDir "repository"
  , workPath       = containerHomePath
  }

getHostPaths :: IO HostPaths
getHostPaths
  =   mkHostPaths
  <$> Path.getCurrentDirectory
  <*> Path.getHomeDirectory
  where
    mkHostPaths :: Path.AbsDir -> Path.AbsDir -> HostPaths
    mkHostPaths hostWorkingPath hostHomePath = HostPaths
      { hostAbuildPath             = hostHomePath       </> Path.relDir ".abuild"
      , hostCachePath              = hostWorkingPath    </> Path.relDir "cache"
      , hostPackagesPath           = hostWorkingPath    </> Path.relDir "packages"
      , hostRepositoryPackagesPath = hostRepositoryPath </> Path.relDir "packages"
      , ..
      }
      where
        hostRepositoryPath :: Path.AbsDir
        hostRepositoryPath = hostWorkingPath </> Path.relDir "repository"

containerAbuildPath :: Path.AbsDir
containerAbuildPath = containerHomePath </> Path.relDir ".abuild"

containerHomePath :: Path.AbsDir
containerHomePath = Path.absDir "/opt/build"

containerCachePath :: Path.AbsDir
containerCachePath = Path.absDir "/var/cache"

containerRepositoryPath :: Path.AbsDir
containerRepositoryPath = containerHomePath </> Path.relDir "repository"

containerPackagesPath :: Path.AbsDir
containerPackagesPath = containerHomePath </> Path.relDir "packages"

run :: String -> [String] -> IO ()
run command arguments = runConfig defaultConfig{arguments = arguments, command = command}

index :: IO ()
index =
  runConfig defaultConfig
    { arguments = ["-F", "index"]
    , command   = "abuild"
    , workPath  = containerPackagesPath </> Path.relDir "stack"
    }

update :: IO ()
update = run "apk" ["update"]

build :: PackageName -> IO ()
build packageName =
  runConfig defaultConfig
    { arguments = ["-r", "-F"]
    , command   = "abuild"
    , workPath  = containerPackagesPath </> Path.relDir (convertText packageName)
    }

runConfig :: Config -> IO ()
runConfig config = runContainer config =<< getHostPaths

runContainer :: Config -> HostPaths -> IO ()
runContainer Config{..} HostPaths{..} = do
  buildImage

  createMissingDirectory hostRepositoryPath
  createMissingDirectory hostPackagesPath
  createMissingDirectory hostAbuildPath
  createMissingDirectory hostCachePath
  createMissingDirectory $ hostCachePath </> Path.relDir "apk"
  createMissingDirectory $ hostCachePath </> Path.relDir "distfiles"
  createMissingDirectory $ hostCachePath </> Path.relDir "misc"

  Process.runProcess_ . Process.proc "podman" $
    [ "run"
    , "--group-add=abuild"
    , "--interactive"
    , "--mount", bindMount hostAbuildPath     containerAbuildPath
    , "--mount", bindMount hostCachePath      containerCachePath
    , "--mount", bindMount hostPackagesPath   containerPackagesPath
    , "--mount", bindMount hostRepositoryPath containerRepositoryPath
    , "--net", "host"
    , "--rm"
    , "--stop-timeout", "0"
    , "--tty"
    , "--workdir", Path.toString workPath
    , "--"
    , convertText imageName
    , command
    ] <> arguments
  where
    buildImage :: IO ()
    buildImage = do
      exists <- testImageExists imageName

      unless exists $
        Process.runProcess_
          . Process.setStdin (Process.byteStringInput dockerfile)
          $ Process.proc "podman"
            [ "build"
            , "--tag", convertText imageName
            , "--file", "-"
            ]

    createMissingDirectory :: Path.AbsDir -> IO ()
    createMissingDirectory = Path.createDirectoryIfMissing False

    bindMount :: Path.AbsDir -> Path.AbsDir -> String
    bindMount source destination =
      List.intercalate
        ","
        [ "type=bind"
        , "source="      <> Path.toString source
        , "destination=" <> Path.toString destination
        ]

    imageName :: ImageName
    imageName =
      convertUnsafe $
        "apk-build-" <> (decodeUtf8 . AWS.sha256Base16 $ AWS.toHashed dockerfile)

#ifndef __HLINT__
    dockerfile = LBS.fromStrict $ encodeUtf8 $$(TH.readFile "Dockerfile")
#endif

testImageExists :: ImageName -> IO Bool
testImageExists imageName = (==) ExitSuccess <$> Process.runProcess process
  where
    process = Process.proc "podman" ["image", "exists", "--", convertText imageName]
