{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      Value (..), object, (.:), (.=))

import Control.Monad (forever, mapM_)

import           Control.Applicative          ((<$>), (<*), (<*>), (<|>))
import           Control.Monad                (when, (>=>))
import           Control.Monad.IO.Class       (liftIO)

import           Control.Concurrent           (forkIO)
import           Data.Aeson                   (Value (..), eitherDecode, encode, decode)
import           Data.Aeson.Types             (typeMismatch)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy.Char8   as BL
import           Data.Maybe                   (maybe)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Lazy.Encoding      (decodeUtf8)

import qualified Aws
import           Aws.Aws                      (Configuration (..))
import qualified Aws.Core                     as Aws
import qualified Aws.S3                       as S3
import qualified Aws.S3.Core                  as S3
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit                 (ResumableSource (..),
                                               unwrapResumable, ($$), ($=))
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import           Network.HTTP.Conduit         (http, parseUrl, responseBody,
                                               withManager)
import Network.AWS.SNS.Publish (publish, Publish, pTargetArn)
import Control.Lens ((.~), view, (^.), (&), (?~))


import Network.AWS (getEnv, Region(NorthVirginia), Credentials(Discover), send, Env, ServiceError, Response)
import Network.AWS.Types (Rs, Sv, Er)
import Control.Monad.Trans.AWS (runAWST)
import Network.AWS.SQS.ReceiveMessage (ReceiveMessage, 
  receiveMessage, rmWaitTimeSeconds, rmMaxNumberOfMessages, 
  ReceiveMessageResponse, rmrMessages)
import Network.AWS.SQS.Types (Message, mBody)
import Network.AWS.SQS (gqurQueueUrl, getQueueUrl)
import Control.Monad.Loops (iterateWhile)
import Data.Maybe (catMaybes)
import Data.Conduit (($=), ($$), awaitForever, runConduit, yield, Source, Conduit, Sink)

import Control.Concurrent (threadDelay)

type Id                 = Int
type Url                = String
type OriginalDirective  = String
type Error              = String

data Directive = Directive {
    dDatasetId            :: Id
  , dRemoteResourceUrl    :: Url
  , dTargetResourceBucket :: Text
  , dTargetResourcePath   :: Text
} deriving Show
instance FromJSON Directive where
  parseJSON (Object v) = Directive
    <$> (v .: "id")
    <*> (v .: "remote_resource_url")
    <*> (v .: "s3_bucket")
    <*> (v .: "s3_path")
  parseJSON o = typeMismatch "Directive" o


data Result = Success Id | Failure OriginalDirective Error deriving Show
instance ToJSON Result where
  toJSON (Success datasetId) =
    object  [
              "status"      .= ("success" :: Text)
            , "id"          .= datasetId
            , "op"          .= ("download" :: Text)
            ]
  toJSON (Failure original err)    =
    object  [
              "status"      .= ("failure" :: Text)
            , "original"    .= original
            , "error"       .= err
            ]

-- {"id": 1, "remote_resource_url": "http//:da", "s3_bucket": "da", "s3_path": "aha"}

notifySNSArn = "arn:aws:sns:us-east-1:445506728970:notify"
{- downloadQueueURL = "https://sqs.us-east-1.amazonaws.com/445506728970/process" -}
downloadQueueURL = "https://sqs.us-east-1.amazonaws.com/445506728970/testQueue"


main :: IO ()
main = do
  {- let dir = Directive 1 "http://samplecsvs.s3.amazonaws.com/TechCrunchcontinentalUSA.csv" "xdataset" "test.csv" -}
  {- print =<< download dir -}
  {- notify ura -}
  env <- getEnv NorthVirginia Discover
  forever $ do
    putStrLn "loop"
    runDaemon env
    threadDelay 1000000

runDaemon :: Env -> IO ()
runDaemon env = forever $ -- runConduit $
  sourceDownloadsFromSQS env $= CL.concatMap id $$ sinkDownloads env

sourceDownloadsFromSQS :: Env -> Source IO [Directive]
sourceDownloadsFromSQS env = forever $ do
  eitherDownloads <- liftIO $ runAWST env $ do
    iterateWhile
      ( (==0) . length )
      $ do 
        liftIO $ do
          threadDelay 1000000
          putStrLn "inner loop"
        
        {- let request = rmMaxNumberOfMessages .~ Just 10 $ rmWaitTimeSeconds .~ Just 10 $ receiveMessage downloadQueueURL  -}
        let request = receiveMessage downloadQueueURL & rmWaitTimeSeconds ?~ 20 
        liftIO $ print request
        {- (eitherResponse :: Either ( ServiceError (Er (Sv ReceiveMessage)) ) ReceiveMessageResponse ) <- send env request -}
        eitherResponse <- send env request
        {- r <- send env $ receiveMessage sqsArn -}
        case eitherResponse of
          Right r -> do
            liftIO $ print r
            return $ r ^. rmrMessages
          Left err -> do
            liftIO $ print err
            return []

  liftIO $ print eitherDownloads
  case eitherDownloads of
    Right downloads -> do
      
      let maybes = map (\dl -> BL.pack . T.unpack <$> dl ^. mBody) downloads
          decodedMaybes = map decode $ catMaybes maybes 

      liftIO $ putStrLn "maybes: "
      liftIO $ print maybes

      yield $ catMaybes decodedMaybes
      {- yield $ catMaybes $ map (\dl -> decode =<< BL.pack . T.unpack <$> dl ^. mBody) downloads -}
    
    Left err -> 
      liftIO $ print err

sinkDownloads :: Env -> Sink Directive IO ()
sinkDownloads env = awaitForever $ \dir -> do
  liftIO $ print dir

notify s = do
  env <- getEnv NorthVirginia Discover  
  runAWST env $ send env $ pTargetArn .~ Just notifySNSArn $ publish s

download :: Directive -> IO Result
download dir@Directive{
    dDatasetId            = did
  , dRemoteResourceUrl    = remoteUrl
  , dTargetResourceBucket = bucket
  , dTargetResourcePath   = path
  } = do
  {- runResourceT $ CB.sourceFile "/Users/ababkin/SacramentocrimeJanuary2006.csv" $= CL.map fixCRs $$ CB.sinkFile "/Users/ababkin/blah.csv" -}

  maybeCreds <- Aws.loadCredentialsFromEnv
  case maybeCreds of
    Nothing -> do
      putStrLn "Please set the environment variables AWS_ACCESS_KEY_ID and AWS_ACCESS_KEY_SECRET"
      return $ Failure "blah" "credentials are not available"
    Just creds -> do
      cfg <- Aws.baseConfiguration
      let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

      request <- parseUrl remoteUrl
      withManager $ \mgr -> do
        resumableSource <- responseBody <$> http request mgr
        (source, _) <- unwrapResumable resumableSource
        let initiator b o = (S3.postInitiateMultipartUpload b o){S3.imuAcl = Just S3.AclPublicRead}
        S3.multipartUploadWithInitiator cfg{credentials = creds} s3cfg{S3.s3Protocol = Aws.HTTPS} initiator mgr bucket path (source $= CL.map fixCRs) (128*1024*1024)

      return $ Success did

fixCRs :: ByteString -> ByteString
fixCRs = BS.map fixCR
  where
    fixCR '\r' = '\n'
    fixCR c = c
