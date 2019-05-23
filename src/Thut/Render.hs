module Thut.Render
  ( renderDocument
  ) where

import           Thut.Prelude

import           Data.List (filter, any)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Thut.Types (CodeblockType(..), Line(..))
import           Thut.Interpreter (EvaluatedDocument(..), EvaluatedBlock(..), EvaluatedLine(..))

-- TODO yeah, so this is fucked up - I need WL PP lib here, it's too intense otherwise
renderDocument :: EvaluatedDocument -> Text
renderDocument = undefined
--renderDocument d =
--  if documentHasErrors d then renderErrors d
--  else renderSuccess d

--documentHasErrors :: EvaluatedDocument -> Bool
--documentHasErrors = any blockHasErrors . evaluatedBlocks
--
--blockHasErrors :: EvaluatedBlock -> Bool
--blockHasErrors = any lineHasErrors . evaluatedBlockLines
--
--lineHasErrors :: EvaluatedLine -> Bool
--lineHasErrors = not . null . evaluatedErrors
--
--renderSuccess :: EvaluatedDocument -> Text
--renderSuccess = unlines' . fmap renderBlocks . evaluatedBlocks
--
--renderBlocks :: EvaluatedBlock -> Text
--renderBlocks = unlines' . \case
--  EvaluatedCodeblock title lines ->
--    ["```" <> renderTitle title] ++ (lines >>= lineAndOutput) ++ ["```"]
--  EvaluatedPlain lines ->
--    lineContents . evaluatedLine <$> lines
--
--lineAndOutput :: EvaluatedLine -> [Text]
--lineAndOutput EvaluatedLine{..} =
--  (lineContents evaluatedLine) : fmap ("-- " <>) evaluatedOutput
--
--renderTitle :: CodeblockType -> Text
--renderTitle = \case
--  Other title -> title
--  ThutSilent -> "thut:silent"
--  ThutPassthrough -> "thut:passthrough"
--  ThutEval -> "thut:eval"
--
--unlines' :: [Text] -> Text
--unlines' = Text.intercalate "\n"
--
---- ERROR RENDERING
--renderErrors :: EvaluatedDocument -> Text
--renderErrors =
--  unlines' . renderRemainingErrors . filterNonErrors
--  where
--    renderRemainingErrors = \case
--      EvaluatedDocument fp (x : xs) ->
--        renderBlockErrors fp Nothing x xs
--      EvaluatedDocument _ [] -> []
--    filterNonErrors d@EvaluatedDocument{..} =
--      d { evaluatedBlocks = filter blockHasErrors evaluatedBlocks }
--
--renderBlockErrors :: FilePath -> Maybe EvaluatedBlock -> EvaluatedBlock -> [EvaluatedBlock] -> [Text]
--renderBlockErrors fp prev current [] =
--  renderFrame fp ++ peekPrev prev ++ myRender current
--renderBlockErrors fp prev current (next : rest) =
--  renderFrame fp ++ peekPrev prev ++ myRender current ++ peekNext next ++ renderBlockErrors fp (Just current) next rest
--
--renderFrame :: FilePath -> [Text]
--renderFrame _ = []
--
--myRender :: EvaluatedBlock -> [Text]
--myRender = \case
--  EvaluatedPlain _ -> []
--  EvaluatedCodeblock title lines ->
--    [renderTitle title] ++ fmap errorLine lines ++ ["```"]
--
--errorLine :: EvaluatedLine -> Text
--errorLine line@EvaluatedLine{..}
--  | lineHasErrors line = lineContents evalauatedLine
--  | otherwise = lineContents evaluatedLine
--
--renderLineNumber :: Int -> Text
--renderLineNumber i =
--  let
--    padding
--      | i < 10 = 2
--      | i < 100 = 1
--      | otherwise = 0
--  in
--    Text.replicate padding " " <> Text.pack (show i) <> " ┃ "
--
--peekPrev :: Maybe EvaluatedBlock -> [Text]
--peekPrev Nothing = []
--peekPrev (Just _) = [] -- FIXME
--
--peekNext :: EvaluatedBlock -> [Text]
--peekNext _ = []

--renderError :: InterpreterConfig -> [Text] -> [Text]
--renderError config output =
--  let
--    splitBlocks current acc (next : rest) =
--      if Text.isPrefixOf "<interactive>:" next then
--        splitBlocks [removePrefix next] (acc ++ [current]) rest
--      else
--        splitBlocks (current ++ [next]) acc rest
--    splitBlocks current acc [] =
--      acc ++ [current]
--
--    removePrefix :: Text -> Text
--    removePrefix line = case Text.splitOn ":" line of
--      (_interactive : _row : _col : _err : rest) ->
--        Text.intercalate ":" rest
--      _ ->
--        line
--
--    errorBlocks =
--      splitBlocks [] [] output
--
--    headMap :: (a -> a) -> [a] -> [a]
--    headMap f (x : xs) = f x : xs
--    headMap _ [] = []
--
--    errors =
--      fmap (headMap Text.stripStart) .
--      fmap (dropWhile Text.null) .
--      fmap (dropWhileEnd Text.null) .
--      filter (not . all (Text.null . Text.strip)) $ errorBlocks
--
--    highlighted =
--      fmap (\line -> red config <> "  ┃ " <> reset config <> line) <$> errors
--
--  in
--    intercalate [] highlighted
--
--red :: InterpreterConfig -> Text
--red config =
--  if configUseColor config then "\x1b[31m" else ""
--
--reset :: InterpreterConfig -> Text
--reset config =
--  if configUseColor config then "\x1b[0m" else ""
