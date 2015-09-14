{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SNS where

import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Network.AWS (send)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.AWS (runAWST)
import qualified Data.ByteString.Lazy.Char8 as BL (unpack)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Network.AWS.SNS.Publish (publish, pTargetARN)
import Control.Lens ((.~), (^.), (&), (?~))
import Control.Monad.Logger (logDebug, logInfo)

import Types (Downloader, Env(..), Directive)
import Util (lookupConfig)


notify :: Directive -> Downloader ()
notify dir = do
  env <- asks amazonkaEnv  
  notifySNSArn <- lookupConfig "DownloadCompleteSNSTopic"

  $logDebug . T.pack $ "Notifying with directive: " ++ show dir
  resp <- liftIO $ runResourceT $ 
            runAWST env $ 
              send $ 
                pTargetARN .~ Just notifySNSArn $ 
                publish . T.pack . BL.unpack $ encode dir
  $logDebug . T.pack $ "SNS response: " ++ show resp

