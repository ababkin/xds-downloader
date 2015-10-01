{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Xds.Downloader.Download where

import Control.Monad.Logger (logInfo)
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8        as BS
import qualified Data.Text as T (pack)

import Xds.Directive.Types (Directive(Directive, results), 
  Param(DownloadParams, remoteUrl, s3Bucket, s3Path), 
  Result(DownloadSuccess, DownloadFail, downloadedUrl))
import Xds.Directive.Util (getDownloadedParams)
import Xds.Aws.S3 (downloadFix)

import Xds.Downloader.Types (Downloader, Env(httpMgr, awsConfig))


{- bucketName :: Text -}
{- bucketName = "xdataset.production" -}

download :: Directive -> Downloader Directive
download dir@Directive{results = rs} =
  case getDownloadedParams dir of
    Just DownloadParams {remoteUrl, s3Bucket, s3Path} -> do
      mgr <- asks httpMgr
      cfg <- asks awsConfig

      $logInfo . T.pack $ "downloading directive: " ++ show dir
      url <- downloadFix mgr cfg remoteUrl s3Bucket s3Path fixCRs
      return $ dir{results = DownloadSuccess{downloadedUrl = url}:rs}

    Nothing ->
      return $ dir{results = (DownloadFail "no download params"):rs} 

fixCRs :: ByteString -> ByteString
fixCRs = BS.map fixCR
  where 
    fixCR '\r' = '\n'
    fixCR c = c

