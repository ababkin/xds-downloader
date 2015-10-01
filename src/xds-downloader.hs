{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Xds.Aws.Config (config)
import Xds.Directive.Types (Directive)

import Xds.Downloader.Types (Downloader(unDownloader), Env(..))
import Xds.Downloader.Queue (getDirectives)
import Xds.Downloader.Notify (notify)
import Xds.Downloader.Download (download)


envVars :: [String]
envVars = ["DownloadQueueName", "DownloadCompleteSNSTopic"]

main :: IO ()
main = do
  
      mgr     <- newManager managerSettings
      awsCfg  <- config mgr
      
      lenv <- M.fromList <$> mapM (\ev -> 
          (T.pack ev,) . T.pack <$> getEnv ev
        ) envVars

      let env = Env awsCfg mgr lenv

      runStdoutLoggingT $ do
        $logDebug . T.pack $ "Environment Variables: " ++ show lenv 
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


