{-# LANGUAGE OverloadedStrings #-}

module SNS where

import Network.AWS (getEnv, Region(NorthVirginia), Credentials(Discover), send, Env, ServiceError, Response)
import Network.AWS.Types (Rs, Sv, Er)

import Control.Monad.Trans.AWS (runAWST)
import           Data.Text                    (Text)

import Network.AWS.SNS.Publish (publish, Publish, pTargetArn)
import Control.Lens ((.~), (^.), (&), (?~))


notifySNSArn = "arn:aws:sns:us-east-1:445506728970:notify"

notify :: Text -> IO ()
notify s = do
  env <- getEnv NorthVirginia Discover  
  runAWST env $ send env $ pTargetArn .~ Just notifySNSArn $ publish s
  return ()

