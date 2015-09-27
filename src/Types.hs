{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Applicative (Applicative)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Map (Map)
import Network.HTTP.Client (Manager)
import GHC.Generics (Generic)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Logger (MonadLogger, LoggingT)

import Xds.Aws.Config (Config)

data Env = Env {
    awsConfig :: Config
  , httpMgr   :: Manager
  , localEnv  :: Map Text Text
  }

newtype Downloader a = 
  Downloader {unDownloader :: ReaderT Env (LoggingT IO) a}
  deriving(Functor, Applicative, Monad, 
    MonadIO, MonadThrow, MonadReader Env, MonadLogger)

type Id   = Int
type URL  = Text
type Path = Text

data Directive = Directive {
    datasetId :: Id
  , remoteUrl :: URL
  , s3Bucket  :: URL
  , s3Path    :: Path
  , logs      :: Maybe [Value]
} deriving (Show, Generic)
instance FromJSON Directive
instance ToJSON Directive

