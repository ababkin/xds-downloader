{-# LANGUAGE OverloadedStrings #-}

module Xds.Downloader.Queue where

import Control.Monad.Reader (asks)

import Data.Text (Text)
import Xds.Aws.SQS (popJson)
import Xds.Directive.Types (Directive)

import Xds.Downloader.Types (Downloader, Env(awsConfig))
import Xds.Downloader.Util (lookupConfig)


awsUserId :: Text
awsUserId = "445506728970"

getDirectives :: Downloader [Directive]
getDirectives = do
  cfg   <- asks awsConfig
  qName <- lookupConfig "DownloadQueueName" 
  popJson cfg qName awsUserId
