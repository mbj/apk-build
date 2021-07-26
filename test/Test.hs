import MPrelude
import Prelude (error)
import Test.Tasty

import qualified APK.StackDeploy          as StackDeploy
import qualified Devtools
import qualified MRIO.Amazonka            as AWS
import qualified StackDeploy.InstanceSpec as InstanceSpec
import qualified StackDeploy.Template     as Template

data Env

instance AWS.HasAWSEnv Env where
  awsEnv _env = error "Intentionally undefined"

instance AWS.HasResourceMap Env where
  resourceMap _env = error "Intentionally undefined"

main :: IO ()
main = do
  devtools <- Devtools.testTree Devtools.defaultConfig
    { Devtools.targets = [Devtools.Target "apk-build"] }

  defaultMain $ testGroup "apk-build" [devtools, template]

template :: TestTree
template
  = Template.testTree
  $ InstanceSpec.templateProvider (StackDeploy.instanceSpecs @Env)
