{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad (forever, mapM)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson (eitherDecode, encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.Conduit.List as CL
import Aws.Ec2.InstanceMetadata (getInstanceMetadataListing)
import qualified Aws
import Network.AWS (newEnv, Region(NorthVirginia), Credentials(Discover))
import qualified Network.AWS as AWS (Env, newLogger, LogLevel(Debug))
import Control.Monad.Loops (iterateWhile)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Conduit (($=), ($$), awaitForever, yield, Source, Conduit, Sink)
import Data.Map (Map)
import qualified Data.Map as M (fromList)
import Network.HTTP.Conduit (withManager, http, parseUrl, responseBody, newManager, 
  ManagerSettings, mkManagerSettings)
import Network.Connection (TLSSettings(TLSSettingsSimple))
import Control.Monad.Except (runExceptT)
import System.Environment (getEnv)
import System.IO (hFlush, stdout)
import Control.Monad.Trans.AWS (envLogger, envManager)
import Aws.Aws (Configuration(credentials))
import Control.Monad.Logger (runStdoutLoggingT, logDebug, logInfo)

import Types (Downloader(unDownloader), Env(..), Directive)
import SQS (getDirectives)
import Notify (notify)
import Download (download)


envVars = ["DownloadQueueName", "DownloadCompleteSNSTopic"]

main :: IO ()
main = do
  
  maybeCreds <- Aws.loadCredentialsFromEnv
  case maybeCreds of
    Nothing ->
      error "Please set the environment variables AWS_ACCESS_KEY_ID and AWS_ACCESS_KEY_SECRET"

    Just creds -> do
      lgr <- AWS.newLogger AWS.Debug stdout  
      httpMgr <- newManager managerSettings
      amazonkaEnv <- newEnv NorthVirginia Discover <&> envLogger .~ lgr <&> envManager .~ httpMgr
      
      config <- M.fromList <$> mapM (\ev -> 
          (T.pack ev,) . T.pack <$> getEnv ev
        ) envVars

      awsConfig <- Aws.baseConfiguration
      {- awsConfig <- Aws.dbgConfiguration -}

      let env = Env awsConfig{credentials = creds} amazonkaEnv httpMgr config


      runStdoutLoggingT $ do
        $logDebug . T.pack $ "Environment Variables: " ++ show config
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
yankDownloadFromQueue = CL.sourceList [1..] 
                $=  CL.mapM popSomeDownloads 
                $=  CL.concatMap id
  where
    popSomeDownloads _ = iterateWhile ( (==0) . length ) getDirectives

performDownload :: Conduit Directive Downloader Directive
performDownload = CL.mapM download

notifyDownloadComplete :: Sink Directive Downloader ()
notifyDownloadComplete = CL.mapM_ notify


