module Thut.Interpreter
  ( interpret
  ) where

import           Thut.Prelude

import           Control.Monad ((<=<), join)
import           Data.Default.Class (Default(..))
import           Data.Either (Either, either)
import           Data.Foldable (foldMap)
import           Data.Functor ((<&>))
import           Data.Maybe (Maybe(..))
import           Data.Text (Text, pack, unpack)
import           Data.Traversable (traverse)
import           Language.Haskell.Ghcid
import           Thut.Types (Block(..), CodeblockType(..), Document(..), Result(..))

data InterpreterConfig = InterpreterConfig
  { configStartCmd :: Text
  }

instance Default InterpreterConfig where
  def = InterpreterConfig { configStartCmd = "cabal v2-repl" }

interpret :: InterpreterConfig -> Document -> IO (Result [Text] [Document])
interpret config (Document fp blocks) =
  foldMap (interpretBlock config) blocks <&> fmap (\b -> [Document fp b])

interpretBlock :: InterpreterConfig -> Block -> IO (Result [Text] [Block])
interpretBlock config = \case
  Markdown lines ->
    pure . Result . pure . Markdown $ lines
  Codeblock blockType@(Other _) contents ->
    pure . Result . pure $ Codeblock blockType contents
  Codeblock ThutEval contents -> fmap pure <$> evalContents config contents

evalContents :: InterpreterConfig -> [Text] -> IO (Result [Text] Block)
evalContents InterpreterConfig{..} xs = do
  (ghci, _) <-
    startGhci (unpack configStartCmd) Nothing (\_ _ -> pure ())

  let
    evalLine line =
      (:) line <$> fmap (pack . (<>) "-- ") <$> exec ghci (unpack line)

  lines <- join <$> traverse evalLine xs
  pure . Result $ Codeblock (Other "haskell") lines
