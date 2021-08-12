module APK.CLI (main) where

import APK.Podman
import APK.Prelude
import Control.Applicative (many)
import Control.Arrow (left)
import Control.Monad (join)

import qualified APK.StackDeploy      as StackDeploy
import qualified Options.Applicative  as Options
import qualified System.Environment   as Environment
import qualified System.IO            as IO
import qualified System.Path          as Path
import qualified System.Process.Typed as Process

main :: IO ()
main = do
  arguments <- Environment.getArgs
  IO.hSetBuffering IO.stdout IO.LineBuffering
  join
    . Options.handleParseResult
    $ Options.execParserPure Options.defaultPrefs commands arguments
  where
    commands :: Options.ParserInfo (IO ())
    commands = Options.info (Options.helper <*> subcommands) Options.idm

    subcommands :: Options.Parser (IO ())
    subcommands
      = Options.hsubparser
      $  Options.command "stack" StackDeploy.parserInfo
      <> mkCommand "build"   (build <$> packageNameParser) "build a package"
      <> mkCommand "index"   (pure index) "regenerate index"
      <> mkCommand "run"     (run <$> stringArgument "COMMAND" <*> many (stringArgument "ARGUMENT")) "run a command"
      <> mkCommand "shell"   (pure $ run "/bin/sh" []) "run a builder shell"
      <> mkCommand "pull-s3" (pure pull) "pull repository from S3"
      <> mkCommand "push-s3" (pure push) "push repository from S3"
      <> mkCommand "update"  (pure update) "update apk repository indexes"

    mkCommand :: String -> Options.Parser a -> String -> Options.Mod Options.CommandFields a
    mkCommand name parser desc = Options.command name (wrapHelper parser desc)

    packageNameParser :: Options.Parser PackageName
    packageNameParser = Options.argument packageNameReader (Options.metavar "PACKAGE_NAME")

    packageNameReader :: Options.ReadM PackageName
    packageNameReader = Options.eitherReader $ left show . convert . convert @Text

    stringArgument :: String -> Options.Parser String
    stringArgument = Options.argument Options.str . Options.metavar

    wrapHelper :: Options.Parser b -> String -> Options.ParserInfo b
    wrapHelper parser desc = Options.info parser (Options.progDesc desc)

push :: IO ()
push = do
  HostPaths{..} <- getHostPaths
  Process.runProcess_ $
    Process.proc
      "aws"
      [ "s3"
      , "sync"
      , "--acl", "public-read"
      , "--delete"
      , "--"
      , Path.toString hostRepositoryPackagesPath
      , "s3://" <> convertText StackDeploy.bucketName
      ]

pull :: IO ()
pull = do
  HostPaths{..} <- getHostPaths
  Process.runProcess_ $
    Process.proc
      "aws"
      [ "s3"
      , "sync"
      , "--"
      , "s3://" <> convertText StackDeploy.bucketName
      , Path.toString hostRepositoryPackagesPath
      ]
