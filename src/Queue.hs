{-# LANGUAGE OverloadedStrings #-}

module Queue where

import Control.Monad.Reader (asks)

import Types (Downloader, Env(awsConfig), Directive)
import Util (lookupConfig)
import Xds.Aws.SQS (popJson)


getDirectives :: Downloader [Directive]
getDirectives = do
  cfg <- asks awsConfig
  qName <- lookupConfig "DownloadQueueName" 
  popJson cfg qName
