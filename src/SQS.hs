{-# LANGUAGE OverloadedStrings #-}

module SQS where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM)
import Data.Text (Text)
import qualified Aws
import qualified Aws.Core as Aws
import qualified Aws.Sqs as Sqs

import Types
import Util


getMessages :: RIO [Text]
getMessages = do
  downloadQueue <- Sqs.QueueName
                    <$> getUserDataValue "DownloadQueueName" 
                    <*> pure "445506728970"
  liftIO $ do
    {- cfg <- Aws.baseConfiguration -}
    cfg <- Aws.dbgConfiguration
    let sqscfg = Sqs.sqs Aws.HTTP Sqs.sqsEndpointUsClassic False :: Sqs.SqsConfiguration Aws.NormalQuery
        receiveMessageReq = Sqs.ReceiveMessage Nothing [] (Just 1) [] downloadQueue (Just 10)

    Sqs.ReceiveMessageResponse msgs <- liftIO $ Aws.simpleAws cfg sqscfg receiveMessageReq
    putStrLn $ "number of messages received: " ++ show (length msgs)
    forM msgs (\msg -> do
      putStrLn $ "   Received json message " ++ show (Sqs.mBody msg)
      Aws.simpleAws cfg sqscfg $ Sqs.DeleteMessage (Sqs.mReceiptHandle msg) downloadQueue 
      return $ Sqs.mBody msg)




