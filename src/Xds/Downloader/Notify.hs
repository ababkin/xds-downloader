{-# LANGUAGE OverloadedStrings #-}

module Xds.Downloader.Notify where

import Control.Monad.Reader (asks)
{- import Control.Monad.Logger (logDebug, logInfo) -}

import Xds.Aws.SNS (publishJson)
import Xds.Directive.Types (Directive)

import Xds.Downloader.Types (Downloader, Env(..))
import Xds.Downloader.Util (lookupConfig)


notify :: Directive -> Downloader ()
notify dir = do
  cfg <- asks awsConfig  
  notifySNSArn <- lookupConfig "DownloadCompleteSNSTopic"
  publishJson cfg notifySNSArn dir
