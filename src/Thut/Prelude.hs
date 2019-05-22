{-# OPTIONS_HADDOCK hide #-}
module Thut.Prelude
  (
  -- * Common operators
    (.)
  , ($)
  , (++)
  , (<$>)
  , (<>)
  , (>>)
  , (>>=)
  -- * Common functions
  , const
  , curry
  , flip
  , fmap
  , fst
  , not
  , null
  , otherwise
  , print
  , read
  , snd
  , uncurry
  , undefined
  -- * Common type classes
  , Applicative(..)
  , Default(..)
  , Eq(..)
  , Num(..)
  , Show(..)
  -- * Common types
  , Bool(..)
  , FilePath
  , Int
  , Integer
  , Char
  , IO
  -- * Text exports
  , Text.putStrLn
  , Text.readFile
  ) where

import           Prelude

import           Control.Applicative (pure)
import           Data.Default.Class (Default(..))
import qualified Data.Text.IO as Text
