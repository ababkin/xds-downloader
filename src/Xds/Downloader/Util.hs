{-# LANGUAGE FlexibleContexts #-}

module Xds.Downloader.Util (lookupConfig) where

import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import qualified Data.Map as M (lookup)
import Data.Text (Text)
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (MonadReader)

import Xds.Downloader.Types (Env(localEnv))


lookupConfig 
  :: (Functor m, MonadReader Env m) 
  => Text
  -> m Text
lookupConfig k = 
  fromJust . M.lookup k <$> asks localEnv
