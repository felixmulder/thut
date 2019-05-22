module Thut.Interpreter
  ( interpret
  ) where

import           Thut.Prelude

import           Control.Monad (void)
import           Data.Foldable (foldMap)
import           Data.Functor ((<&>))
import           Data.IORef (modifyIORef, newIORef, readIORef)
import           Data.List (all, dropWhile, dropWhileEnd, intercalate, filter)
import           Data.Maybe (Maybe(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable (traverse)
import           Language.Haskell.Ghcid (Ghci, Stream(..), execStream, startGhci, stopGhci)
import           Thut.Types (Document(..), Block(..), CodeblockType(..))
import           Thut.Types (InterpreterConfig(..), Result(..))

interpret :: InterpreterConfig -> Document -> IO (Result Document Document)
interpret config (Document fp unparsedBlocks) = withGhci config $ \ghci ->
  foldMap (interpretBlock config ghci) unparsedBlocks <&> \case
    Result blocks -> Result $ Document fp blocks
    Errors blocks -> Errors $ Document fp blocks

interpretBlock :: InterpreterConfig -> Ghci -> Block -> IO (Result [Block] [Block])
interpretBlock config ghci = \case
  Markdown lines ->
    pure . Result . pure . Markdown $ lines
  Codeblock ThutEval contents ->
    evalContents config ghci contents <&> \case
      Result blocks -> Result [blocks]
      Errors errors -> Errors [Codeblock ThutEval errors]
  Codeblock ThutPassthrough contents ->
    evalPassthrough config ghci contents <&> \case
      Result blocks -> Result [blocks]
      Errors errors -> Errors [Codeblock ThutEval errors]
  Codeblock ThutSilent contents -> do
    evalContents config ghci contents <&> \case
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

evalPassthrough :: InterpreterConfig -> Ghci -> [Text] -> IO (Result [Text] Block)
evalPassthrough config ghci xs = do
  let
    consolidateResults :: Result Interpreted Interpreted -> Result [Text] [Text]
    consolidateResults (Result Interpreted{..}) =
      Result interpretedResult
    consolidateResults (Errors (Interpreted input output)) =
      Errors $ input : renderError config output

  interpreteds <- traverse (evalLine ghci) xs
  pure $ Markdown <$> foldMap consolidateResults interpreteds

evalContents :: InterpreterConfig -> Ghci -> [Text] -> IO (Result [Text] Block)
evalContents config ghci xs = do
  let
    consolidateResults :: Result Interpreted Interpreted -> Result [Text] [Text]
    consolidateResults (Result (Interpreted input output)) =
      Result $ input : fmap ("-- " <>) output
    consolidateResults (Errors (Interpreted input output)) =
      Errors $ input : renderError config output

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
renderError :: InterpreterConfig -> [Text] -> [Text]
renderError config output =
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
      fmap (\line -> red config <> "  ┃ " <> reset config <> line) <$> errors

  in
    intercalate [] highlighted

red :: InterpreterConfig -> Text
red config =
  if configUseColor config then "\x1b[31m" else ""

reset :: InterpreterConfig -> Text
reset config =
  if configUseColor config then "\x1b[0m" else ""
