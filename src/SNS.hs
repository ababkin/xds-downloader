{-# LANGUAGE OverloadedStrings #-}

module SNS where

import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Network.AWS (getEnv, Region(NorthVirginia), Credentials(Discover), send, Env, ServiceError, Response)
import Network.AWS.Types (Rs, Sv, Er)
import Aws.Ec2.InstanceMetadata (getInstanceMetadataListing)
import           Network.HTTP.Conduit         (withManager)

import Control.Monad.Trans.AWS (runAWST)
import           Data.Text                    (Text)

import Network.AWS.SNS.Publish (publish, Publish, pTargetArn)
import Control.Lens ((.~), (^.), (&), (?~))

import Types
import Util


notify :: Text -> RIO ()
notify s = do
  env <- asks awsEnv  
  notifySNSArn <- getUserDataValue "DownloadCompleteSNSTopic"
  liftIO $ putStrLn $ "    Notifying: " ++ show s
  resp <- liftIO $ runAWST env $ send env $ pTargetArn .~ Just notifySNSArn $ publish s
  liftIO $ putStrLn $ "   SNS response: " ++ show resp
  return ()

