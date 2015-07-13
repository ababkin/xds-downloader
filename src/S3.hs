module S3 where

import Control.Applicative ((<$>))
import qualified Aws
import           Aws.Aws                      (Configuration (..))
import qualified Aws.Core                     as Aws
import qualified Aws.S3                       as S3
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as BS
import           Network.HTTP.Conduit         (http, parseUrl, responseBody, withManager)
import           Data.Conduit                 (unwrapResumable, ($=))
import qualified Data.Conduit.List            as CL

import Types

download :: Directive -> IO Result
download Directive{
    dDatasetId            = did
  , dRemoteResourceUrl    = remoteUrl
  , dTargetResourceBucket = bucket
  , dTargetResourcePath   = path
  } = do

  maybeCreds <- Aws.loadCredentialsFromEnv
  case maybeCreds of
    Nothing -> do
      putStrLn "Please set the environment variables AWS_ACCESS_KEY_ID and AWS_ACCESS_KEY_SECRET"
      return $ Failure "blah" "credentials are not available"
    Just creds -> do
      cfg <- Aws.baseConfiguration
      let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

      request <- parseUrl remoteUrl
      withManager $ \mgr -> do
        resumableSource <- responseBody <$> http request mgr
        (source, _) <- unwrapResumable resumableSource
        let initiator b o = (S3.postInitiateMultipartUpload b o){S3.imuAcl = Just S3.AclPublicRead}
        S3.multipartUploadWithInitiator cfg{credentials = creds} s3cfg{S3.s3Protocol = Aws.HTTPS} initiator mgr bucket path (source $= CL.map fixCRs) (128*1024*1024)

      return $ Success did

  where

    fixCRs :: ByteString -> ByteString
    fixCRs = BS.map fixCR
      where
        
        fixCR '\r' = '\n'
        fixCR c = c

