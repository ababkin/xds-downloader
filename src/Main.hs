{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Applicative ((<$>), (<*>), pure)

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.Aeson                   (eitherDecode, encode, decode)
import qualified Data.ByteString.Lazy.Char8   as BL
import           Data.Text                    (Text)
import qualified Data.Text                    as T

{- import           Control.Monad.Trans.Resource (runResourceT) -}
import qualified Data.Conduit.List            as CL
import Aws.Ec2.InstanceMetadata (getInstanceMetadataListing)

import Network.AWS (newEnv, Region(NorthVirginia), Credentials(Discover))
import qualified Network.AWS as AWS (Env)
import Control.Monad.Loops (iterateWhile)
import Data.Maybe (catMaybes)
import Data.Conduit (($=), ($$), awaitForever, yield, Source, Conduit, Sink)
import           Network.HTTP.Conduit         (withManager)
import Data.Map (Map)
import qualified Data.Map as M (fromList)
import Network.HTTP.Conduit         (http, parseUrl, responseBody, newManager, ManagerSettings, mkManagerSettings)
import Network.Connection (TLSSettings(TLSSettingsSimple))
import Control.Monad.Except (runExceptT)

import Types
import SQS (getMessages)
import SNS (notify)
import S3 (download)


main :: IO ()
main = do
  
  mgr <- newManager managerSettings
  (Right amazonkaEnv) <- runExceptT $ newEnv NorthVirginia Discover mgr
  ud <- fmap toUserDataMap $ liftIO $ getInstanceMetadataListing mgr "latest/user-data"

  let env = Env mgr amazonkaEnv ud

  putStr "UserData: "
  print $ userData env

  runReaderT runDaemon env
  
  where
    toUserDataMap :: [String] -> Map Text Text
    toUserDataMap = M.fromList . map (\t -> 
        let k:v:_ = T.split (=='=') $ T.pack t in (k, v)
      )

    managerSettings :: ManagerSettings
    managerSettings = mkManagerSettings tlsSettings sockSettings
      where
        tlsSettings = TLSSettingsSimple True False False
        sockSettings = Nothing

runDaemon :: RIO ()
runDaemon = forever $
  sourceDownloadsFromSQS 
    $= CL.concatMap id 
    $= conduitDownload
    $$ sinkNotifyDownload

sourceDownloadsFromSQS :: Source RIO [Directive]
sourceDownloadsFromSQS = forever $ do
  messages <- lift $ iterateWhile ( (==0) . length ) getMessages
  yield $ catMaybes $ map decode $ map (BL.pack . T.unpack) messages

conduitDownload :: Conduit Directive RIO Directive
conduitDownload = awaitForever $ \dir -> do
  lift $ download dir
  yield dir

sinkNotifyDownload :: Sink Directive RIO ()
sinkNotifyDownload = awaitForever $ \dir -> do
  lift $ notify $ T.pack $ show dir


