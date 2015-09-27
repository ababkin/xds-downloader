{-# LANGUAGE OverloadedStrings #-}

module Queue where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (pack)
import qualified Aws
import qualified Aws.Core as Aws
import qualified Aws.Sqs as Sqs
import Data.Maybe (catMaybes)
import Aws.Sqs.Commands.Message (Message(Message, mBody))
import Data.Aeson (eitherDecode)
import Control.Monad.Logger (logDebug, logInfo, logWarn)

import Types (Downloader, Env(awsConfig), Directive)
import Util (lookupConfig)
import Xds.Aws.SQS (popJson)

getDirectives :: Downloader [Directive]
getDirectives = do
  cfg <- asks awsConfig
  qName <- lookupConfig "DownloadQueueName" 
  popJson cfg qName
