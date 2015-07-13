module Util where

import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import qualified Data.Map as M (lookup)
import Data.Text (Text)
import Control.Monad.Trans.Reader (asks)

import Types


getUserDataValue 
  :: Text
  -> RIO Text
getUserDataValue k = 
  fromJust . M.lookup k <$> asks userData
