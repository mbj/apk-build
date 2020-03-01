module APK.StackDeploy (bucketName, instanceSpecs, parserInfo) where

import APK.AWS
import APK.Prelude
import Options.Applicative (ParserInfo)
import StackDeploy.Template (Template)
import Stratosphere hiding (Template, template)
import System.Exit (exitWith)

import qualified StackDeploy.CLI          as StackDeploy
import qualified StackDeploy.InstanceSpec as InstanceSpec
import qualified StackDeploy.Template     as Template
import qualified Stratosphere

newtype BucketName = BucketName Text
  deriving newtype ToText

parserInfo :: (MonadCatch m, MonadUnliftIO m) => ParserInfo (m ())
parserInfo = (liftIO . exitWith <=< withEnv) <$>
  StackDeploy.parserInfo instanceSpecs

instanceSpecs :: InstanceSpec.Provider
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
bucketName = BucketName "mbj-apk"
