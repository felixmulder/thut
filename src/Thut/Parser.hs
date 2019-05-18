module Thut.Parser
  ( parseDocument
  ) where

import Thut.Prelude

import Control.Monad (mapM_)
import Control.Monad.Trans.State.Lazy
import Data.Maybe (Maybe)
import Data.Text (Text, drop, isPrefixOf, lines, strip, stripEnd)
import Thut.Types

parseDocument :: FilePath -> Text -> Document
parseDocument fp =
  Document fp
  . parsedBlocks
  . flip execState initialParsed
  . mapM_ parseLine
  . fmap stripEnd
  . lines

data Parsed = Parsed
  { currentBlock :: Block
  , parsedBlocks :: [Block]
  }

initialParsed :: Parsed
initialParsed = Parsed (Markdown []) []

parseTitle :: Text -> CodeblockType
parseTitle title = case strip title of
  "thut:eval" -> ThutEval
  other -> Other other

parseLine :: Text -> State Parsed ()
parseLine line =
  if "```" `isPrefixOf` line then
    toggleCodeblock (drop 3 line)
  else
    addLine line

addLine :: Text -> State Parsed ()
addLine line = get >>= \parsed ->
  let
    newBlock = case currentBlock parsed of
      Markdown lines -> Markdown $ lines ++ [line]
      Codeblock title lines -> Codeblock title $ lines ++ [line]
  in
    put $ parsed { currentBlock = newBlock }

toggleCodeblock :: Text -> State Parsed ()
toggleCodeblock title = do
  Parsed current parsed <- get

  let
    newCurrent = case current of
      Markdown _ -> Codeblock (parseTitle title) []
      Codeblock _ _ -> Markdown []

  put $ Parsed newCurrent (parsed ++ [current])
