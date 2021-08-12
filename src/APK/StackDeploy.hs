module APK.StackDeploy (bucketName, instanceSpecs, parserInfo) where

import APK.Prelude
import Options.Applicative (ParserInfo)
import StackDeploy.Template (Template)
import Stratosphere hiding (Template, template)

import qualified APK.AWS                  as AWS
import qualified MRIO.Amazonka            as AWS
import qualified MRIO.Log                 as Log
import qualified StackDeploy.CLI          as StackDeploy
import qualified StackDeploy.Config       as StackDeploy
import qualified StackDeploy.InstanceSpec as InstanceSpec
import qualified StackDeploy.Template     as Template
import qualified Stratosphere
import qualified System.Exit              as System

type BucketName = BoundText "BucketName"

parserInfo :: ParserInfo (IO ())
parserInfo = run <$> StackDeploy.parserInfo instanceSpecs
  where
    run :: RIO Env System.ExitCode -> IO a
    run app = withEnv $ \env -> System.exitWith =<< runRIO env app

instanceSpecs :: InstanceSpec.Provider env
instanceSpecs =
  [ InstanceSpec.mk (InstanceSpec.mkName "apk-repository") template
  ]

template :: Template
template
  = Template.mk (Template.mkName "repository")
  $ Stratosphere.template [repository]
  & templateOutputs ?~ outputs
  where
    outputs = Outputs
      [ output "RepositoryBucketName"
         (toRef repository)
         & outputExport ?~ OutputExport "ApkRepositoryBucketName"
      ]

repository :: Resource
repository
  = resource "Repository"
  $ s3Bucket
  & sbAccessControl ?~ Literal PublicRead
  & sbBucketName    ?~ Literal (convertText bucketName)

bucketName :: BucketName
bucketName = fromType @"mbj-apk"

data Env = Env
  { awsEnv      :: AWS.Env
  , logAction   :: Log.Action (RIO Env)
  , resourceMap :: AWS.ResourceMap
  , stackDeploy :: StackDeploy.Config Env
  }

data Boot = Boot
  { awsEnv      :: AWS.Env
  , resourceMap :: AWS.ResourceMap
  }

instance AWS.HasResourceMap Boot where
  resourceMap Boot{..} = resourceMap

instance AWS.HasAWSEnv Boot where
  awsEnv Boot{..} = awsEnv

instance AWS.HasResourceMap Env where
  resourceMap Env{..} = resourceMap

instance AWS.HasAWSEnv Env where
  awsEnv Env{..} = awsEnv

instance StackDeploy.HasConfig Env where
  getConfig = stackDeploy

withEnv :: (Env -> IO a) -> IO a
withEnv action = do
  AWS.withResourceMap $ \resourceMap -> do
    awsEnv <- AWS.getEnv

    action Env
      { logAction   = Log.defaultCLIAction
      , stackDeploy = StackDeploy.defaultConfig, ..
      }
