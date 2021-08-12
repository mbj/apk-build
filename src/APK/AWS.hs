module APK.AWS (getEnv) where

import APK.Prelude
import Control.Lens (set)
import Network.AWS
import System.IO (stderr)

getEnv :: (MonadCatch m, MonadUnliftIO m) => m Env
getEnv = do
  logger <- newLogger Info stderr
  newEnv Discover <&> set envLogger logger <&> set envRegion NorthVirginia
