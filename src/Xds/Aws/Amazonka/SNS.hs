{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Xds.Aws.Amazonka.SNS (publishJson) where

import qualified Data.ByteString.Lazy.Char8 as BL (unpack)
import Control.Lens ((.~))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (logDebug, MonadLogger)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.AWS (runAWST)
import Data.Aeson (encode, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Network.AWS (send)
import Network.AWS.SNS.Publish (publish, pTargetARN)
import qualified Network.AWS as Amazonka (Env)

import Xds.Aws.Internal.Config (Config(Config, amazonkaConfig))


publishJson
  :: (MonadIO m, 
      MonadLogger m, 
      ToJSON obj, 
      Show obj) 
  => Config 
  -> Text 
  -> obj 
  -> m ()
publishJson Config{amazonkaConfig} arn obj = do
  $logDebug . T.pack $ "Publishing json object to SNS: " ++ show obj
  resp <- liftIO $ runResourceT $ 
            runAWST amazonkaConfig $ 
              send $ 
                pTargetARN .~ Just arn $ 
                publish . T.pack . BL.unpack $ encode obj 
  $logDebug . T.pack $ "SNS response: " ++ show resp
