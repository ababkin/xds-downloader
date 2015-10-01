{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Xds.Downloader.Types where

import Control.Applicative (Applicative)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Map (Map)
import Network.HTTP.Client (Manager)
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

