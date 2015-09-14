{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module SQS where

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


getDirectives :: Downloader [Directive]
getDirectives = do
  downloadQueue <- Sqs.QueueName
                    <$> lookupConfig "DownloadQueueName" 
                    <*> pure "445506728970"

  cfg <- asks awsConfig
  let sqscfg = Sqs.sqs Aws.HTTP Sqs.sqsEndpointUsClassic False :: Sqs.SqsConfiguration Aws.NormalQuery
      receiveMessageReq = Sqs.ReceiveMessage Nothing [] (Just 1) [] downloadQueue (Just 10)

  Sqs.ReceiveMessageResponse msgs <- Aws.simpleAws cfg sqscfg receiveMessageReq

  $logDebug . T.pack $ "number of messages received: " ++ show (length msgs)

  catMaybes <$> forM msgs (\msg@Message{mBody} -> do
      $logDebug . T.pack $ "received json: " ++ show mBody
      case eitherDecode . BL.pack . T.unpack $ mBody of
        Right dir -> do
          $logDebug . T.pack $ "decoded directive: " ++ show dir 
          Aws.simpleAws cfg sqscfg $ Sqs.DeleteMessage (Sqs.mReceiptHandle msg) downloadQueue 
          return $ Just dir
        Left err -> do
          $logWarn . T.pack $ "failed to decode decoded directive due to: " ++ show err 
          return Nothing
    )




