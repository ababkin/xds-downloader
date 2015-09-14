{-# LANGUAGE FlexibleContexts #-}

module Xds.Aws.S3 (
  downloadFix
  ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Applicative ((<$>))
import Data.Text (unpack)
import qualified Aws
import qualified Aws.Core                     as Aws
import qualified Aws.S3                       as S3
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8        as BS
import Network.HTTP.Conduit (http, parseUrl, responseBody, newManager, ManagerSettings, mkManagerSettings)
import Data.Conduit (unwrapResumable, ($=))
import qualified Data.Conduit.List            as CL
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Logger (logDebug, logInfo)

import Types (URL, Path, Env(..))

downloadFix 
  :: (MonadReader Env m, MonadIO m)
  => URL
  -> URL
  -> Path
  -> (ByteString -> ByteString) 
  -> m ()
downloadFix url s3Bucket s3Path f = do
  mgr <- asks httpMgr
  cfg <- asks awsConfig

  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  liftIO $ runResourceT $ do
    request <- parseUrl $ unpack url

    resumableSource <- responseBody <$> http request mgr
    (source, _) <- unwrapResumable resumableSource
    let initiator b o = (S3.postInitiateMultipartUpload b o){S3.imuAcl = Just S3.AclPublicRead}
    S3.multipartUploadWithInitiator cfg s3cfg{S3.s3Protocol = Aws.HTTPS} 
      initiator mgr s3Bucket s3Path (source $= CL.map f) (128*1024*1024)
