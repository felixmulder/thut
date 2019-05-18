module Thut
  ( eval
  ) where

import Thut.Prelude

import Data.Text (Text)
import Thut.Types (Document, Result)

eval :: Text -> IO (Result [Text] Document)
eval = undefined
