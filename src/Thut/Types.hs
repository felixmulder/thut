module Thut.Types where

import Thut.Prelude

import Control.Applicative (Applicative(..))
import Data.Functor (Functor(..))
import Data.Maybe (Maybe)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)

data Document = Document
  { documentPath :: FilePath
  , documentBlocks :: [Block]
  }
  deriving (Eq, Show)

data Block
  = Markdown [Text]
  | Codeblock
    { blockTitle :: CodeblockType
    , blockContents :: [Text]
    }
  deriving (Eq, Show)

data CodeblockType
  = ThutEval
  | Other Text
  deriving (Eq, Show)

-- | Small datatype that collects errors in its semigroup instance
data Result e a
  = Result a
  | Errors e
  deriving (Eq, Show)

instance (Semigroup a, Semigroup e) => Semigroup (Result e a) where
  Result a1 <> Result a2 = Result (a1 <> a2)
  Errors xs <> Errors ys = Errors (xs <> ys)
  Errors xs <> _         = Errors xs
  _         <> Errors xs = Errors xs

instance (Semigroup e, Monoid a) => Monoid (Result e a) where
  mempty = Result mempty

instance Functor (Result e) where
  fmap f (Result a) = Result (f a)
  fmap _ (Errors e) = Errors e

instance Semigroup e => Applicative (Result e) where
  pure = Result
  liftA2 f (Result a)  (Result b)  = Result (f a b)
  liftA2 f (Errors xs) (Errors ys) = Errors (xs <> ys)
  liftA2 f (Errors xs) _           = Errors xs
  liftA2 f _           (Errors xs) = Errors xs
