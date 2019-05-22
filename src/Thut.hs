module Thut
  ( evalText
  , evalText'
  ) where

import Thut.Prelude

import Control.Monad ((<=<))
import Data.Bifunctor (first, second)
import Data.Default.Class (def)
import Data.Either (Either(..))
import Data.Text (Text)
import Thut.Interpreter (interpret)
import Thut.Parser (parseDocument)
import Thut.Render (renderDocument)
import Thut.Types (Document, InterpreterConfig, toEither)

-- TODO: Should we expose this?
--evalDocument :: FilePath -> Text -> IO (Either Text Document)
--evalDocument = evalDocument' def

evalDocument' :: InterpreterConfig -> FilePath -> Text -> IO (Either Text Document)
evalDocument' config fp contents = do
  result <- interpret config $ parseDocument fp contents
  pure . first renderDocument $ toEither result

evalText :: Text -> IO (Either Text Text)
evalText = evalText' def

evalText' :: InterpreterConfig -> Text -> IO (Either Text Text)
evalText' config =
  pure . second renderDocument <=< evalDocument' config ""
