{-# OPTIONS_HADDOCK hide #-}
module Thut.Prelude
  (
  -- * Common operators
    (.)
  , ($)
  , (++)
  , (<$>)
  -- * Common functions
  , const
  , curry
  , flip
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
  , Ord(..)
  , Functor(..)
  , Monad(..)
  , Monoid(..)
  , Num(..)
  , Semigroup(..)
  , Show(..)
  -- * Common types
  , Bool(..)
  , Char
  , FilePath
  , IO
  , Int
  , Integer
  , Maybe(..)
  -- * Text exports
  , Text.putStrLn
  , Text.readFile
  ) where

import           Prelude

import           Data.Default.Class (Default(..))
import qualified Data.Text.IO as Text
