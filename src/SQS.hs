{-# LANGUAGE OverloadedStrings #-}

module SQS where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM)
import           Data.Text                    (Text)
import qualified Aws
import qualified Aws.Core                     as Aws
import qualified Aws.Sqs as Sqs


-- {"id": 1, "remote_resource_url": "http://samplecsvs.s3.amazonaws.com/SalesJan2009.csv", "s3_bucket": "xdataset", "s3_path": "aha.csv"}

downloadQueue = Sqs.QueueName "testQueue" "445506728970"

getMessages = do
  cfg <- Aws.baseConfiguration
  let sqscfg = Sqs.sqs Aws.HTTP Sqs.sqsEndpointUsClassic False :: Sqs.SqsConfiguration Aws.NormalQuery
      receiveMessageReq = Sqs.ReceiveMessage Nothing [] (Just 1) [] downloadQueue (Just 10)

  Sqs.ReceiveMessageResponse msgs <- liftIO $ Aws.simpleAws cfg sqscfg receiveMessageReq
  putStrLn $ "number of messages received: " ++ show (length msgs)
  forM msgs (\msg -> do
    putStrLn $ "   Received " ++ show (Sqs.mBody msg)
    Aws.simpleAws cfg sqscfg $ Sqs.DeleteMessage (Sqs.mReceiptHandle msg) downloadQueue 
    return $ Sqs.mBody msg)

getQueues :: IO [Text]
getQueues = do
  {- List any Queues you have already created in your SQS account -}
  cfg <- Aws.baseConfiguration
  let sqscfg = Sqs.sqs Aws.HTTP Sqs.sqsEndpointUsClassic False :: Sqs.SqsConfiguration Aws.NormalQuery
  
  Sqs.ListQueuesResponse qUrls <- Aws.simpleAws cfg sqscfg $ Sqs.ListQueues Nothing
  return qUrls

