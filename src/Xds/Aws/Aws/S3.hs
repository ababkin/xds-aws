{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Xds.Aws.Aws.S3 (
  downloadFix
  ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Applicative ((<$>))
import Data.Text (Text)
import qualified Data.Text as T (unpack, concat)
import qualified Aws
import qualified Aws.Core                     as Aws
import qualified Aws.S3                       as S3
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8        as BS
import Network.HTTP.Conduit (http, parseUrl, responseBody, newManager, ManagerSettings, mkManagerSettings)
import Data.Conduit (unwrapResumable, ($=))
import qualified Data.Conduit.List            as CL
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (logDebug, logInfo)
import Network.HTTP.Client (Manager)

import Xds.Aws.Types (URL, Path)
import Xds.Aws.Internal.Config (Config(Config, awsConfig))


downloadFix 
  :: (MonadIO m)
  => Manager
  -> Config 
  -> URL
  -> Text
  -> Path
  -> (ByteString -> ByteString) 
  -> m URL
downloadFix mgr Config{awsConfig} url s3Bucket s3Path f = do

  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  liftIO $ runResourceT $ do
    request <- parseUrl $ T.unpack url

    resumableSource <- responseBody <$> http request mgr
    (source, _) <- unwrapResumable resumableSource
    let initiator b o = (S3.postInitiateMultipartUpload b o){S3.imuAcl = Just S3.AclPublicRead}
    S3.multipartUploadWithInitiator awsConfig s3cfg{S3.s3Protocol = Aws.HTTPS} 
      initiator mgr s3Bucket s3Path (source $= CL.map f) (128*1024*1024)

  return $ T.concat ["https://s3.amazonaws.com/", s3Bucket, "/", s3Path]
