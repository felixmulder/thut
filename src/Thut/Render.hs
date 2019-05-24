module Thut.Render
  ( renderDocument
  , renderDocument'
  , documentHasErrors
  , blockHasErrors
  , lineHasErrors
  ) where

import           Thut.Prelude

import qualified Data.List as List
import           Data.List (filter, any, all)
import           Data.Text (Text)
import           Data.Default.Class (def)
import qualified Data.Text as Text
import           Thut.Types (CodeblockType(..), InterpreterConfig(..), Line(..), Symbols(..))
import           Thut.Interpreter (EvaluatedDocument(..), EvaluatedBlock(..), EvaluatedLine(..))

renderDocument :: EvaluatedDocument -> Text
renderDocument = renderDocument' def

renderDocument' :: InterpreterConfig -> EvaluatedDocument -> Text
renderDocument' config doc =
  if documentHasErrors doc then renderErrors config doc
  else renderSuccess doc

documentHasErrors :: EvaluatedDocument -> Bool
documentHasErrors = any blockHasErrors . evaluatedBlocks

blockHasErrors :: EvaluatedBlock -> Bool
blockHasErrors = any lineHasErrors . evaluatedBlockLines

lineHasErrors :: EvaluatedLine -> Bool
lineHasErrors = not . null . evaluatedErrors

renderSuccess :: EvaluatedDocument -> Text
renderSuccess = unlines' . (>>= renderBlocks) . evaluatedBlocks

renderBlocks :: EvaluatedBlock -> [Text]
renderBlocks = \case
  EvaluatedCodeblock ThutPassthrough lines ->
    lines >>= evaluatedOutput
  EvaluatedCodeblock blockType lines ->
    renderedWith blockType $ lines >>= lineAndOutput
  EvaluatedPlain lines ->
    lineContents . evaluatedLine <$> lines

lineAndOutput :: EvaluatedLine -> [Text]
lineAndOutput EvaluatedLine{..} =
  (lineContents evaluatedLine) : fmap ("-- " <>) evaluatedOutput

renderedWith :: CodeblockType -> [Text] -> [Text]
renderedWith blockType lines =
  case blockType of
    ThutSilent -> []
    ThutPassthrough -> lines
    ThutEval -> ["```haskell"] ++ lines ++ ["```"]
    Other title -> ["```" <> title] ++ lines ++ ["```"]

unlines' :: [Text] -> Text
unlines' = Text.intercalate "\n"

-- ERROR RENDERING
renderErrors :: InterpreterConfig -> EvaluatedDocument -> Text
renderErrors config =
  unlines' . renderRemainingErrors . filterNonErrors
  where
    renderRemainingErrors = \case
      EvaluatedDocument fp (x : xs) ->
        renderBlockErrors config fp Nothing x xs
      EvaluatedDocument _ [] -> []
    filterNonErrors d@EvaluatedDocument{..} =
      d { evaluatedBlocks = filter blockHasErrors evaluatedBlocks }

renderBlockErrors :: InterpreterConfig -> FilePath -> Maybe EvaluatedBlock -> EvaluatedBlock -> [EvaluatedBlock] -> [Text]
renderBlockErrors config fp prev current [] =
  renderFrame config fp ++ peekPrev prev ++ myRender config current
renderBlockErrors config fp prev current (next : rest) =
  renderFrame config fp ++
  peekPrev prev ++
  myRender config current ++
  peekNext next ++
  renderBlockErrors config fp (Just current) next rest

renderFrame :: InterpreterConfig -> FilePath -> [Text]
renderFrame _ _ = []

myRender :: InterpreterConfig -> EvaluatedBlock -> [Text]
myRender config = \case
  EvaluatedPlain _ -> []
  EvaluatedCodeblock blockType lines ->
    [renderBlockType blockType] ++ (lines >>= errorLine config) ++ ["```"]

renderBlockType :: CodeblockType -> Text
renderBlockType = \case
  ThutSilent -> "```thut:silent"
  ThutEval -> "```thut:eval"
  ThutPassthrough -> "```thut:passthrough"
  Other title -> "```" <> title

errorLine :: InterpreterConfig -> EvaluatedLine -> [Text]
errorLine config line@EvaluatedLine{..}
  | lineHasErrors line = lineContents evaluatedLine : renderError config evaluatedErrors
  | otherwise = [lineContents evaluatedLine]

-- FIXME: we should enable this
--renderLineNumber :: Int -> Text
--renderLineNumber i =
--  let
--    padding
--      | i < 10 = 2
--      | i < 100 = 1
--      | otherwise = 0
--  in
--    Text.replicate padding " " <> Text.pack (show i) <> " ┃ "

peekPrev :: Maybe EvaluatedBlock -> [Text]
peekPrev Nothing = []
peekPrev (Just _) = [] -- FIXME

peekNext :: EvaluatedBlock -> [Text]
peekNext _ = []

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

    errors :: [[Text]]
    errors =
      fmap (headMap Text.stripStart) .
      fmap (List.dropWhile Text.null) .
      fmap (List.dropWhileEnd Text.null) .
      filter (not . all (Text.null . Text.strip)) $ errorBlocks

    prefixLine =
      if configSymbols config == Fancy then
        "  ┃ "
      else
        "  | "

    highlighted =
      fmap (\line -> red config <> prefixLine <> reset config <> line) <$> errors

  in
    List.intercalate [] highlighted

red :: InterpreterConfig -> Text
red config =
  if configUseColor config then "\x1b[31m" else ""

reset :: InterpreterConfig -> Text
reset config =
  if configUseColor config then "\x1b[0m" else ""
