{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Download where

import Data.Text (pack)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8        as BS
import Control.Monad.Logger (logDebug, logInfo)

import Xds.Aws.S3 (downloadFix)
import Types (Downloader(..), 
  Directive(Directive, remoteUrl, s3Path, logs),
  Env(..))


s3Bucket = "xdataset.production"

download :: Directive -> Downloader Directive
download dir@Directive{remoteUrl, s3Path, logs} = do
  $logDebug . pack $ "downloading directive: " ++ show dir
  downloadFix remoteUrl s3Bucket s3Path fixCRs
  return $ dir{logs = logs}

fixCRs :: ByteString -> ByteString
fixCRs = BS.map fixCR
  where 
    fixCR '\r' = '\n'
    fixCR c = c

