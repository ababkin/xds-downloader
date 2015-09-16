{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Xds.Aws.SQS where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (MonadLogger, logDebug, logInfo, logWarn)
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (MonadReader)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (pack)
import qualified Aws
import qualified Aws.Core as Aws
import qualified Aws.Sqs as Sqs
import Data.Maybe (catMaybes)
import Aws.Sqs.Commands.Message (Message(Message, mBody))
import Data.Aeson (eitherDecode, FromJSON)

import Types (Env(awsConfig))


popJson
  :: (Functor m, 
      MonadIO m, 
      MonadReader Env m, 
      MonadLogger m, 
      FromJSON obj, 
      Show obj) 
  => Text 
  -> m [obj]
popJson qName = do
  let q = Sqs.QueueName qName "445506728970"

  cfg <- asks awsConfig
  let sqscfg = Sqs.sqs Aws.HTTP Sqs.sqsEndpointUsClassic False :: Sqs.SqsConfiguration Aws.NormalQuery
      receiveMessageReq = Sqs.ReceiveMessage Nothing [] (Just 1) [] q (Just 10)

  Sqs.ReceiveMessageResponse msgs <- Aws.simpleAws cfg sqscfg receiveMessageReq

  $logDebug . T.pack $ "number of messages received: " ++ show (length msgs)

  catMaybes <$> forM msgs (\msg@Message{mBody} -> do
      $logDebug . T.pack $ "received json: " ++ show mBody
      case eitherDecode . BL.pack . T.unpack $ mBody of
        Right obj -> do
          $logDebug . T.pack $ "decoded json object: " ++ show obj 
          Aws.simpleAws cfg sqscfg $ Sqs.DeleteMessage (Sqs.mReceiptHandle msg) q 
          return $ Just obj
        Left err -> do
          $logWarn . T.pack $ "failed to decode decoded json object due to: " ++ show err 
          return Nothing
    )



