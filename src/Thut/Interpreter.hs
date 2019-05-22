module Thut.Interpreter
  ( interpret
  ) where

import           Thut.Prelude

import           Control.Monad ((<=<), join, forM_, void)
import           Data.Default.Class (def)
import           Data.Either (Either(..), either, isLeft)
import           Data.Foldable (any, foldMap)
import           Data.Functor ((<&>))
import           Data.IORef (modifyIORef, newIORef, readIORef)
import           Data.List (all, dropWhile, dropWhileEnd, intercalate, filter)
import           Data.Maybe (Maybe(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable (traverse)
import           Language.Haskell.Ghcid (Ghci, Stream(..), execStream, startGhci, stopGhci)
import           Thut.Types (Document(..), Block(..), CodeblockType(..))
import           Thut.Types (InterpreterConfig(..), Result(..), isError)

interpret :: InterpreterConfig -> Document -> IO (Result Document Document)
interpret config (Document fp blocks) = withGhci config $ \ghci ->
  foldMap (interpretBlock ghci) blocks <&> \case
    Result blocks -> Result $ Document fp blocks
    Errors blocks -> Errors $ Document fp blocks

interpretBlock :: Ghci -> Block -> IO (Result [Block] [Block])
interpretBlock ghci = \case
  Markdown lines ->
    pure . Result . pure . Markdown $ lines
  Codeblock ThutEval contents ->
    evalContents ghci contents <&> \case
      Result blocks -> Result [blocks]
      Errors errors -> Errors [Codeblock ThutEval errors]
  Codeblock ThutPassthrough contents ->
    evalPassthrough ghci contents <&> \case
      Result blocks -> Result [blocks]
      Errors errors -> Errors [Codeblock ThutEval errors]
  Codeblock ThutSilent contents -> do
    evalContents ghci contents <&> \case
      Result _ -> Result []
      Errors errors -> Errors [Codeblock ThutEval errors]
  Codeblock blockType@(Other _) contents ->
    pure . Result . pure $ Codeblock blockType contents

withGhci :: InterpreterConfig -> (Ghci -> IO a) -> IO a
withGhci InterpreterConfig{..} f = do
  (ghci, _) <- startGhci (Text.unpack configStartCmd) Nothing (\_ _ -> pure ())
  a <- f ghci
  stopGhci ghci
  pure a

data Interpreted = Interpreted
  { interpretedText :: Text
  , interpretedResult :: [Text]
  }

evalPassthrough :: Ghci -> [Text] -> IO (Result [Text] Block)
evalPassthrough ghci xs = do
  let
    consolidateResults :: Result Interpreted Interpreted -> Result [Text] [Text]
    consolidateResults (Result (Interpreted _ output)) = Result output
    consolidateResults (Errors (Interpreted input output)) =
      Errors $ input : renderError output

  interpreteds <- traverse (evalLine ghci) xs
  pure $ Markdown <$> foldMap consolidateResults interpreteds

evalContents :: Ghci -> [Text] -> IO (Result [Text] Block)
evalContents ghci xs = do
  let
    consolidateResults :: Result Interpreted Interpreted -> Result [Text] [Text]
    consolidateResults (Result (Interpreted input output)) =
      Result $ input : fmap ("-- " <>) output
    consolidateResults (Errors (Interpreted input output)) =
      Errors $ input : renderError output

  interpreteds <- traverse (evalLine ghci) xs
  pure $ Codeblock (Other "haskell") <$> foldMap consolidateResults interpreteds

evalLine :: Ghci -> Text -> IO (Result Interpreted Interpreted)
evalLine ghci cmd = do
  errors <- newIORef []
  output <- newIORef []

  void . execStream ghci (Text.unpack cmd) $ \stream s ->
    modifyIORef (if stream == Stdout then output else errors) (<> [Text.pack s])

  finalErrors <- readIORef errors

  if null finalErrors then
    Result . Interpreted cmd <$> readIORef output
  else
    pure . Errors $ Interpreted cmd finalErrors


-- Error rendering
--
renderError :: [Text] -> [Text]
renderError output =
  let
    splitBlocks current acc (next : rest) =
      if Text.isPrefixOf "<interactive>:" next then
        splitBlocks [removePrefix next] (acc ++ [current]) rest
      else
        splitBlocks (current ++ [next]) acc rest
    splitBlocks current acc [] =
      acc ++ [current]

    removePrefix :: Text -> Text
    removePrefix line = case Text.splitOn ":" line of
      (_interactive : _row : _col : _err : rest) ->
        Text.intercalate ":" rest
      _ ->
        line

    errorBlocks =
      splitBlocks [] [] output

    headMap :: (a -> a) -> [a] -> [a]
    headMap f (x : xs) = f x : xs
    headMap _ [] = []

    errors =
      fmap (headMap Text.stripStart) .
      fmap (dropWhile Text.null) .
      fmap (dropWhileEnd Text.null) .
      filter (not . all (Text.null . Text.strip)) $ errorBlocks

    highlighted =
      fmap (\line -> red <> "  ┃ " <> reset <> line) <$> errors

  in
    intercalate [] highlighted

red :: Text
red = "\x1b[31m"

reset :: Text
reset = "\x1b[0m"
