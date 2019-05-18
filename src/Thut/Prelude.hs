module Thut.Prelude
  (
  -- * Common operators
    (.)
  , ($)
  , (++)
  , (<$>)
  , (>>)
  , (>>=)
  , (<>)
  -- * Common functions
  , const
  , curry
  , flip
  , fmap
  , fst
  , print
  , pure
  , snd
  , uncurry
  , undefined
  -- * Common type classes
  , Default(..)
  , Eq(..)
  , Show(..)
  -- * Common types
  , Bool(..)
  , FilePath
  , IO
  -- * Text exports
  , Text.putStrLn
  , Text.readFile
  ) where

import           Prelude

import           Control.Applicative (pure)
import           Data.Text (Text)
import           Data.Default.Class (Default(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Environment as Env
