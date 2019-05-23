module Thut.Types where

import Thut.Prelude

import Data.Default.Class (Default(..))
import Data.Text (Text)

data InterpreterConfig = InterpreterConfig
  { configStartCmd :: Text
  , configUseColor :: Bool
  }

instance Default InterpreterConfig where
  def = InterpreterConfig
    { configStartCmd = "cabal v2-repl"
    , configUseColor = True
    }

data Document = Document
  { documentPath :: FilePath
  , documentBlocks :: [Block]
  }
  deriving (Eq, Show)

data Line = Line
  { lineNumber :: Int
  , lineContents :: Text
  }
  deriving (Eq, Show)

data Block
  = Markdown [Line]
  | Codeblock
    { blockTitle :: CodeblockType
    , blockContents :: [Line]
    }
  deriving (Eq, Show)

data CodeblockType
  = ThutEval
  | ThutPassthrough
  | ThutSilent
  | Other Text
  deriving (Eq, Show)
