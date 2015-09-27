{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (forever)
import Control.Applicative ((<$>))
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Text as T (pack)
import qualified Data.Conduit.List as CL
{- import Aws.Ec2.InstanceMetadata (getInstanceMetadataListing) -}
import Control.Monad.Loops (iterateWhile)
import Data.Conduit (($=), ($$), Source, Conduit, Sink)
import qualified Data.Map as M (fromList)
import Network.HTTP.Conduit (newManager, 
  ManagerSettings, mkManagerSettings)
import Network.Connection (TLSSettings(TLSSettingsSimple))
import System.Environment (getEnv)
import Control.Monad.Logger (runStdoutLoggingT, logDebug)

import Xds.Amazonka.Config as Xds.Amazonka (config)
import Xds.Aws.Config as Xds.Aws (config)

import Types (Downloader(unDownloader), Env(..), Directive)
import Queue (getDirectives)
import Notify (notify)
import Download (download)


envVars :: [String]
envVars = ["DownloadQueueName", "DownloadCompleteSNSTopic"]

main :: IO ()
main = do
  
      mgr       <- newManager managerSettings
      amazonka  <- Xds.Amazonka.config mgr
      aws       <- Xds.Aws.config
      
      cfg <- M.fromList <$> mapM (\ev -> 
          (T.pack ev,) . T.pack <$> getEnv ev
        ) envVars

      let env = Env aws amazonka mgr cfg 

      runStdoutLoggingT $ do
        $logDebug . T.pack $ "Environment Variables: " ++ show cfg
        runReaderT (unDownloader runDaemon) env
  
  where
    managerSettings :: ManagerSettings
    managerSettings = mkManagerSettings tlsSettings sockSettings
      where
        tlsSettings = TLSSettingsSimple True False False
        sockSettings = Nothing

runDaemon :: Downloader ()
runDaemon = forever $
        yankDownloadFromQueue 
    $=  performDownload
    $$  notifyDownloadComplete

yankDownloadFromQueue :: Source Downloader Directive
yankDownloadFromQueue = CL.sourceList ([1..] :: [Int])
                $=  CL.mapM popSomeDownloads 
                $=  CL.concatMap id
  where
    popSomeDownloads _ = iterateWhile ( (==0) . length ) getDirectives

performDownload :: Conduit Directive Downloader Directive
performDownload = CL.mapM download

notifyDownloadComplete :: Sink Directive Downloader ()
notifyDownloadComplete = CL.mapM_ notify


