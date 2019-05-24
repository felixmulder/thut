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
  , parsedLine :: Int
  }

getBlocks :: Parsed -> [Block]
getBlocks Parsed{..} = parsedBlocks ++ [currentBlock]

initialParsed :: Parsed
initialParsed = Parsed (Markdown []) [] 1

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
addLine inputLine = get >>= \Parsed{..}->
  let
    line = Line parsedLine inputLine
    newBlock = case currentBlock of
      Markdown lines -> Markdown $ lines ++ [line]
      Codeblock title lines -> Codeblock title $ lines ++ [line]
  in
    put $ Parsed newBlock parsedBlocks (parsedLine + 1)

toggleCodeblock :: Text -> State Parsed ()
toggleCodeblock title = get >>= \(Parsed current parsed line) ->
  let
    newCurrent = case current of
      Markdown _ -> Codeblock (parseTitle title) []
      Codeblock _ _ -> Markdown []
  in
    put $ Parsed newCurrent (parsed ++ [current]) (line + 1)
