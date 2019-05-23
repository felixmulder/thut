module Thut.Parser
  ( parseDocument
  ) where

import Thut.Prelude

import           Control.Monad (mapM_)
import           Control.Monad.State.Lazy (State, execState, get, put)
import           Data.Text (Text, isPrefixOf)
import qualified Data.Text as Text
import           Thut.Types

parseDocument :: FilePath -> Text -> Document
parseDocument fp =
  Document fp
  . getBlocks
  . flip execState initialParsed
  . mapM_ parseLine
  . fmap Text.stripEnd
  . Text.lines

data Parsed = Parsed
  { currentBlock :: Block
  , parsedBlocks :: [Block]
  }

getBlocks :: Parsed -> [Block]
getBlocks Parsed{..} = parsedBlocks ++ [currentBlock]

initialParsed :: Parsed
initialParsed = Parsed (Markdown []) []

parseTitle :: Text -> CodeblockType
parseTitle title = case Text.strip title of
  "thut:eval" -> ThutEval
  "thut:passthrough" -> ThutPassthrough
  "thut:silent" -> ThutSilent
  other -> Other other

parseLine :: Text -> State Parsed ()
parseLine line =
  if "```" `isPrefixOf` line then
    toggleCodeblock (Text.drop 3 line)
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
