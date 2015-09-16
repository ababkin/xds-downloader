{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Xds.Amazonka.SNS where

import qualified Data.ByteString.Lazy.Char8 as BL (unpack)
import Control.Lens ((.~), (^.), (&), (?~))
import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (logDebug, logInfo, MonadLogger)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.AWS (runAWST)
import Data.Aeson (encode, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Network.AWS (send)
import Network.AWS.SNS.Publish (publish, pTargetARN)

import Types (Env(..))

publishJson
  :: (MonadIO m, MonadReader Env m, MonadLogger m, ToJSON obj, Show obj) 
  => Text 
  -> obj 
  -> m ()
publishJson arn obj = do
  env <- asks amazonkaEnv  

  $logDebug . T.pack $ "Publishing json object to SNS: " ++ show obj
  resp <- liftIO $ runResourceT $ 
            runAWST env $ 
              send $ 
                pTargetARN .~ Just arn $ 
                publish . T.pack . BL.unpack $ encode obj 
  $logDebug . T.pack $ "SNS response: " ++ show resp
