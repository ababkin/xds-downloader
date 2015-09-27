{-# LANGUAGE OverloadedStrings #-}

module Notify where

import Control.Monad.Reader (asks)
{- import Control.Monad.Logger (logDebug, logInfo) -}

import Types (Downloader, Env(..), Directive)
import Util (lookupConfig)
import Xds.Amazonka.SNS (publishJson)


notify :: Directive -> Downloader ()
notify dir = do
  env <- asks amazonkaEnv  
  notifySNSArn <- lookupConfig "DownloadCompleteSNSTopic"
  publishJson env notifySNSArn dir
