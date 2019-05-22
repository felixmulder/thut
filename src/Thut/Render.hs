module Thut.Render
  ( renderDocument
  , renderBlock
  ) where

import Thut.Prelude

import Data.Text (Text, intercalate)
import Thut.Types (Document(..), Block(..), CodeblockType(..))

renderDocument :: Document -> Text
renderDocument = intercalate "\n" . fmap renderBlock . documentBlocks

renderBlock :: Block -> Text
renderBlock = \case
  Codeblock title xs ->
    unlines' $ ["```" <> renderTitle title] ++ xs ++ ["```"]
  Markdown xs ->
    unlines' xs

renderTitle :: CodeblockType -> Text
renderTitle = \case
  Other title -> title
  ThutSilent -> "thut:silent"
  ThutPassthrough -> "thut:passthrough"
  ThutEval -> "thut:eval"

unlines' :: [Text] -> Text
unlines' = intercalate "\n"
