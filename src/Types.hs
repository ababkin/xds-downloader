{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson (Value (..), eitherDecode, encode, decode)
import Data.Aeson.Types (typeMismatch)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON),
  Value (..), object, (.:), (.=))
import Data.Text (Text)
import Control.Applicative ((<$>), (<*), (<*>), (<|>))

type Id                 = Int
type Url                = String
type OriginalDirective  = String
type Error              = String

data Directive = Directive {
    dDatasetId            :: Id
  , dRemoteResourceUrl    :: Url
  , dTargetResourceBucket :: Text
  , dTargetResourcePath   :: Text
} deriving Show
instance FromJSON Directive where
  parseJSON (Object v) = Directive
    <$> (v .: "id")
    <*> (v .: "remote_resource_url")
    <*> (v .: "s3_bucket")
    <*> (v .: "s3_path")
  parseJSON o = typeMismatch "Directive" o


data Result = Success Id | Failure OriginalDirective Error deriving Show
instance ToJSON Result where
  toJSON (Success datasetId) =
    object  [
              "status"      .= ("success" :: Text)
            , "id"          .= datasetId
            , "op"          .= ("download" :: Text)
            ]
  toJSON (Failure original err)    =
    object  [
              "status"      .= ("failure" :: Text)
            , "original"    .= original
            , "error"       .= err
            ]
