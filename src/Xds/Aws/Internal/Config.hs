module Xds.Aws.Internal.Config (Config(Config, awsConfig, amazonkaConfig), config) where

import Control.Lens
import Network.AWS (newEnv, Region(NorthVirginia), Credentials(Discover))
import Control.Monad.Trans.AWS (envLogger, envManager)
import qualified Network.AWS as AWS (Env, newLogger, LogLevel(Debug))
import System.IO (stdout)
import Network.HTTP.Client (Manager)
import qualified Aws (baseConfiguration, dbgConfiguration)
import Aws.Aws (Configuration)


data Config = Config {
    awsConfig       :: Configuration
  , amazonkaConfig  :: AWS.Env
  }

config :: Manager -> IO Config
config httpManager = do
  -- Amazonka
  logger <- AWS.newLogger AWS.Debug stdout  
  amazonkaCfg <- newEnv NorthVirginia Discover <&> envLogger .~ logger <&> envManager .~ httpManager

  -- Aws
  {- awsCfg <- Aws.baseConfiguration -}
  awsCfg <- Aws.dbgConfiguration
  

  return Config {
      awsConfig       = awsCfg
    , amazonkaConfig  = amazonkaCfg
    }
