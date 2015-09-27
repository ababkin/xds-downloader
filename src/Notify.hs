{-# LANGUAGE OverloadedStrings #-}

module Notify where

import Control.Monad.Reader (asks)
{- import Control.Monad.Logger (logDebug, logInfo) -}

import Types (Downloader, Env(..), Directive)
import Util (lookupConfig)
import Xds.Aws.SNS (publishJson)


notify :: Directive -> Downloader ()
notify dir = do
  cfg <- asks awsConfig  
  notifySNSArn <- lookupConfig "DownloadCompleteSNSTopic"
  publishJson cfg notifySNSArn dir
