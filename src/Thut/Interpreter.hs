module Thut.Interpreter
  ( interpret
  ) where

import           Thut.Prelude

import           Control.Monad ((<=<), join)
import           Data.Either (Either, either)
import           Data.Foldable (foldMap)
import           Data.Functor ((<&>))
import           Data.Maybe (Maybe(..))
import           Data.Text (Text, pack, unpack)
import           Data.Traversable (traverse)
import           Language.Haskell.Ghcid
import           Thut.Types (Block(..), CodeblockType(..), Document(..), Result(..))

interpret :: Document -> IO (Result [Text] [Document])
interpret (Document fp blocks) =
  foldMap interpretBlock blocks <&> fmap (\b -> [Document fp b])

interpretBlock :: Block -> IO (Result [Text] [Block])
interpretBlock = \case
  Markdown lines ->
    pure . Result . pure . Markdown $ lines
  Codeblock blockType@(Other _) contents ->
    pure . Result . pure $ Codeblock blockType contents
  Codeblock ThutEval contents -> fmap pure <$> evalContents contents

evalContents :: [Text] -> IO (Result [Text] Block)
evalContents xs = do
  ghci <- fst <$> startGhci "ghci" Nothing (\_ _ -> pure ())

  let
    evalLine line =
      (:) line <$> fmap (pack . (<>) "-- ") <$> exec ghci (unpack line)

  lines <- join <$> traverse evalLine xs
  pure . Result $ Codeblock (Other "haskell") lines
