module APK.AWS
  ( module Control.Lens.Getter
  , module Control.Lens.Lens
  , module Control.Lens.Setter
  , AWSConstraint
  , MonadAWS
  , withEnv
  )
where

import APK.Prelude
import Control.Lens.Getter (view)
import Control.Lens.Lens ((&), (<&>))
import Control.Lens.Setter ((.~), (?~), set)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.AWS (AWSConstraint, runAWST)
import Control.Monad.Trans.Resource (runResourceT)
import Network.AWS (AWS, MonadAWS)

import qualified Network.AWS as AWS
import qualified System.IO   as IO

withEnv :: (MonadCatch m, MonadUnliftIO m) => AWS a -> m a
withEnv action = do
  logger <- AWS.newLogger AWS.Info IO.stderr
  env    <- AWS.newEnv AWS.Discover <&> set AWS.envLogger logger

  liftIO . runResourceT $ runAWST env $ AWS.within AWS.NorthVirginia action
