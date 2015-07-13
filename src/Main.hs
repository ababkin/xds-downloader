{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)

import           Data.Aeson                   (eitherDecode, encode, decode)
import qualified Data.ByteString.Lazy.Char8   as BL
import           Data.Text                    (Text)
import qualified Data.Text                    as T

{- import           Control.Monad.Trans.Resource (runResourceT) -}
import qualified Data.Conduit.List            as CL

import Network.AWS (getEnv, Region(NorthVirginia), Credentials(Discover), Env)
import Control.Monad.Loops (iterateWhile)
import Data.Maybe (catMaybes)
import Data.Conduit (($=), ($$), awaitForever, yield, Source, Conduit, Sink)

import Types
import SQS (getMessages)
import SNS (notify)
import S3 (download)


main :: IO ()
main = do
  env <- getEnv NorthVirginia Discover
  runDaemon env

runDaemon :: Env -> IO ()
runDaemon env = forever $
  sourceDownloadsFromSQS 
    $= CL.concatMap id 
    $= conduitDownload
    $$ sinkNotifyDownload env

sourceDownloadsFromSQS :: Source IO [Directive]
sourceDownloadsFromSQS = forever $ do
  messages <- liftIO $ iterateWhile ( (==0) . length ) getMessages
  yield $ catMaybes $ map decode $ map (BL.pack . T.unpack) messages

conduitDownload :: Conduit Directive IO Directive
conduitDownload = awaitForever $ \dir -> do
  liftIO $ download dir
  yield dir

sinkNotifyDownload :: Env -> Sink Directive IO ()
sinkNotifyDownload env = awaitForever $ \dir -> do
  liftIO $ notify $ T.pack $ show dir


