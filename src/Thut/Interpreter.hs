module Thut.Interpreter
  ( EvaluatedDocument(..)
  , EvaluatedBlock(..)
  , EvaluatedLine(..)
  , interpret
  ) where

import           Thut.Prelude

import           Control.Monad (void)
import           Data.Foldable (foldMap)
import           Data.Functor ((<&>))
import           Data.IORef (modifyIORef, newIORef, readIORef)
import           Data.Maybe (Maybe(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable (traverse)
import           Language.Haskell.Ghcid (Ghci, Stream(..), execStream, startGhci, stopGhci)
import           Thut.Types (Document(..), Block(..), CodeblockType(..), Line(..))
import           Thut.Types (InterpreterConfig(..))

data EvaluatedDocument = EvaluatedDocument
  { evaluatedPath :: FilePath
  , evaluatedBlocks :: [EvaluatedBlock]
  }

data EvaluatedBlock
  = EvaluatedPlain
    { evaluatedBlockLines :: [EvaluatedLine] }
  | EvaluatedCodeblock
    { evaluatedBlockType :: CodeblockType
    , evaluatedBlockLines :: [EvaluatedLine]
    }

data EvaluatedLine = EvaluatedLine
  { evaluatedLine :: Line
  , evaluatedOutput :: [Text]
  , evaluatedErrors :: [Text]
  }

interpret :: InterpreterConfig -> Document -> IO EvaluatedDocument
interpret config (Document fp unparsedBlocks) = withGhci config $ \ghci ->
  foldMap (interpretBlock ghci) unparsedBlocks <&> EvaluatedDocument fp

interpretBlock :: Ghci -> Block -> IO [EvaluatedBlock]
interpretBlock ghci = \case
  Markdown lines ->
    pure [EvaluatedPlain (evalPure <$> lines)]
  Codeblock blockType@(Other _) contents ->
    pure [EvaluatedCodeblock blockType (evalPure <$> contents)]
  Codeblock blockType contents ->
    evalContents ghci contents <&> pure . EvaluatedCodeblock blockType

withGhci :: InterpreterConfig -> (Ghci -> IO a) -> IO a
withGhci InterpreterConfig{..} f = do
  (ghci, _) <- startGhci (Text.unpack configStartCmd) Nothing (\_ _ -> pure ())
  a <- f ghci
  stopGhci ghci
  pure a

evalPure :: Line -> EvaluatedLine
evalPure line = EvaluatedLine line [] []

evalContents :: Ghci -> [Line] -> IO [EvaluatedLine]
evalContents ghci xs = traverse (evalLine ghci) xs

evalLine :: Ghci -> Line -> IO EvaluatedLine
evalLine ghci line@(Line _ cmd) = do
  errors <- newIORef []
  output <- newIORef []

  void . execStream ghci (Text.unpack cmd) $ \stream s ->
    modifyIORef (if stream == Stdout then output else errors) (<> [Text.pack s])

  EvaluatedLine line <$> readIORef output <*> readIORef errors
