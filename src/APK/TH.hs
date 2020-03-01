module APK.TH (readFile) where

import APK.Prelude
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax
import System.FilePath (FilePath)

import qualified Data.Text.IO as Text

readFile :: FilePath -> Q (TExp Text)
readFile path = do
  qAddDependentFile path
  TExp <$> (lift =<< runIO (Text.readFile path))
