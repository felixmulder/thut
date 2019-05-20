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
import           Data.List (dropWhileEnd)
import           Data.Maybe (Maybe(..))
import           Data.Text (Text, intercalate, pack, strip, unpack)
import           Data.Traversable (traverse)
import           Language.Haskell.Ghcid
import           Thut.Types (Document(..), Block(..), CodeblockType(..))
import           Thut.Types (InterpreterConfig(..), Result(..), isError)

interpret :: InterpreterConfig -> Document -> IO (Result Document Document)
interpret config (Document fp blocks) =
  foldMap (interpretBlock config) blocks <&> \case
    Result blocks -> Result $ Document fp blocks
    Errors blocks -> Errors $ Document fp blocks

interpretBlock :: InterpreterConfig -> Block -> IO (Result [Block] [Block])
interpretBlock config = \case
  Markdown lines ->
    pure . Result . pure . Markdown $ lines
  Codeblock blockType@(Other _) contents ->
    pure . Result . pure $ Codeblock blockType contents
  Codeblock ThutEval contents ->
    evalContents config contents <&> \case
      Result blocks -> Result [blocks]
      Errors errors -> Errors [Codeblock ThutEval errors]

withGhci :: InterpreterConfig -> (Ghci -> IO a) -> IO a
withGhci InterpreterConfig{..} f = do
  (ghci, _) <- startGhci (unpack configStartCmd) Nothing (\_ _ -> pure ())
  a <- f ghci
  stopGhci ghci
  pure a

data Interpreted = Interpreted
  { interpretedText :: Text
  , interpretedResult :: [Text]
  }

evalContents :: InterpreterConfig -> [Text] -> IO (Result [Text] Block)
evalContents config xs = withGhci config $ \ghci -> do
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

  void . execStream ghci (unpack cmd) $ \stream s ->
    modifyIORef (if stream == Stdout then output else errors) (<> [pack s])

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
    lines = dropWhileEnd ((== "") . strip) output
  in
    pure . (<> reset) . intercalate "\n" $
      fmap (red <>) lines

red :: Text
red = "\x1b[31m"

reset :: Text
reset = "\x1b[0m"
