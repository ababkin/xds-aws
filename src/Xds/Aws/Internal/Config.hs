module Xds.Aws.Internal.Config (Config(Config, awsConfig, amazonkaConfig), config) where

import Control.Lens
import Network.AWS (newEnv, Region(NorthVirginia), Credentials(Discover))
import Control.Monad.Trans.AWS (envLogger, envManager)
import qualified Network.AWS as AWS (Env, newLogger, LogLevel(Debug))
import System.IO (stdout)
import Network.HTTP.Client (Manager)
import qualified Aws (loadCredentialsFromEnv, baseConfiguration)
import Aws.Aws (Configuration(credentials))


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
  maybeCreds <- Aws.loadCredentialsFromEnv
  awsCfg <- maybe
    (error "Please set the environment variables AWS_ACCESS_KEY_ID and AWS_ACCESS_KEY_SECRET")
    (\creds -> do
      cfg <- Aws.baseConfiguration
      return cfg{credentials = creds}
      {- Aws.dbgConfiguration{credentials = creds} -}
    )
    maybeCreds

  return Config {
      awsConfig       = awsCfg
    , amazonkaConfig  = amazonkaCfg
    }
