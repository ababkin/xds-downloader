{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Download where

import Control.Monad.Logger (logDebug, logInfo)
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8        as BS
import Data.Text (pack)

import Xds.Aws.S3 (downloadFix)
import Types (Downloader(..), Env(httpMgr, awsConfig),
  Directive(Directive, remoteUrl, s3Path, logs))


s3Bucket = "xdataset.production"

download :: Directive -> Downloader Directive
download dir@Directive{remoteUrl, s3Path, logs} = do
  mgr <- asks httpMgr
  cfg <- asks awsConfig

  $logDebug . pack $ "downloading directive: " ++ show dir
  downloadFix mgr cfg remoteUrl s3Bucket s3Path fixCRs
  return $ dir{logs = logs}

fixCRs :: ByteString -> ByteString
fixCRs = BS.map fixCR
  where 
    fixCR '\r' = '\n'
    fixCR c = c

