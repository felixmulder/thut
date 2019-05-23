module Thut
  ( -- * Eval functions
    evalFile
  , evalText
  , evalDocument

    -- * Re-exports
  , parseDocument
  , renderDocument
  , InterpreterConfig(..)
  , EvaluatedDocument(..)
  , EvaluatedBlock(..)
  , EvaluatedLine(..)
  ) where

import Thut.Prelude

import Data.Default.Class (def)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Thut.Interpreter (interpret)
import Thut.Parser (parseDocument)
import Thut.Render (renderDocument)
import Thut.Types (Document, InterpreterConfig(..))
import Thut.Interpreter (EvaluatedDocument(..), EvaluatedBlock(..), EvaluatedLine(..))

evalFile :: FilePath -> IO EvaluatedDocument
evalFile = evalFile' def

evalFile' :: InterpreterConfig -> FilePath -> IO EvaluatedDocument
evalFile' config fp = Text.readFile fp >>= evalText' config fp

evalText :: FilePath -> Text -> IO EvaluatedDocument
evalText fp = evalDocument . parseDocument fp

evalText' :: InterpreterConfig -> FilePath -> Text -> IO EvaluatedDocument
evalText' config fp = evalDocument' config . parseDocument fp

evalDocument :: Document -> IO EvaluatedDocument
evalDocument = evalDocument' def

evalDocument' :: InterpreterConfig -> Document -> IO EvaluatedDocument
evalDocument' = interpret
