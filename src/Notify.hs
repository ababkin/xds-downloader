{-# LANGUAGE OverloadedStrings #-}

module Notify where

import qualified Data.ByteString.Lazy.Char8 as BL (unpack)
import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebug, logInfo)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.AWS (runAWST)
import Data.Text (Text)
import qualified Data.Text as T (pack)

import Types (Downloader, Env(..), Directive)
import Util (lookupConfig)
import Xds.Amazonka.SNS (publishJson)


notify :: Directive -> Downloader ()
notify dir = do
  env <- asks amazonkaEnv  
  notifySNSArn <- lookupConfig "DownloadCompleteSNSTopic"
  publishJson env notifySNSArn dir
