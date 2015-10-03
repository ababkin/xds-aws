{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Xds.Aws.Aws.S3 (
  downloadFix
  ) where

import qualified Aws
import qualified Aws.Core                     as Aws
import qualified Aws.S3                       as S3
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (MonadLogger, logDebug, logInfo)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8        as BS
import Data.Conduit (unwrapResumable, ($=))
import qualified Data.Conduit.List            as CL
import Data.Text (Text)
import qualified Data.Text as T (unpack, concat, append)
import Network.HTTP.Client (Manager)
import Network.HTTP.Conduit (http, parseUrl, responseBody, newManager, ManagerSettings, mkManagerSettings)

import Xds.Aws.Types (URL, Path)
import Xds.Aws.Internal.Config (Config(Config, awsConfig))


downloadFix 
  :: (MonadIO m, MonadLogger m)
  => Manager
  -> Config 
  -> URL
  -> URL
  -> Path
  -> (ByteString -> ByteString) 
  -> m URL
downloadFix mgr Config{awsConfig} url s3BucketUrl s3FilenameWithPath f = do

  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  $logDebug $ T.concat ["downloading URL: ", url, " into S3 bucket: ", s3BucketUrl, " filepath: ", s3FilenameWithPath, " ..."]
  liftIO $ runResourceT $ do

    request <- parseUrl $ T.unpack url

    resumableSource <- responseBody <$> http request mgr
    (source, _) <- unwrapResumable resumableSource
    let initiator b o = (S3.postInitiateMultipartUpload b o){S3.imuAcl = Just S3.AclPublicRead}
    S3.multipartUploadWithInitiator awsConfig s3cfg{S3.s3Protocol = Aws.HTTPS} 
      initiator mgr s3BucketUrl s3FilenameWithPath (source $= CL.map f) (128*1024*1024)

  let downloadedUrl = T.concat ["https://s3.amazonaws.com/", s3BucketUrl, "/", s3FilenameWithPath]
  $logDebug $ "... finished downloading, download URL: " `T.append` downloadedUrl
  return downloadedUrl
