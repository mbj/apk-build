import MPrelude
import Test.Tasty

import qualified APK.StackDeploy          as StackDeploy
import qualified Devtools
import qualified StackDeploy.InstanceSpec as InstanceSpec
import qualified StackDeploy.Template     as Template

main :: IO ()
main =
  defaultMain $
    testGroup
      "apk-build"
        [ Devtools.testTree Devtools.defaultConfig
        , template
        ]

template :: TestTree
template
  = Template.testTree
  $ InstanceSpec.templateProvider StackDeploy.instanceSpecs
