module Main (main) where

import Thut.Prelude

import Control.Monad (forM_)
import Data.Either (Either)
import Data.Foldable (foldMap)
import Data.List (zip)
import Data.Text (Text, stripEnd, isPrefixOf, drop, intercalate)
import Data.Traversable (traverse)
import System.Environment (getArgs)
import Thut.Parser (parseDocument)
import Thut.Types (Block(..), CodeblockType(..), Document(..), Result(..))
import Thut.Interpreter (interpret)

main :: IO ()
main = do
  filePaths <- getArgs
  fileContents <- traverse readFile filePaths

  foldMap interpret (uncurry parseDocument <$> zip filePaths fileContents)
    >>= reportErrors
    >>= writeOutput

reportErrors :: Result [Text] [Document] -> IO (Result [Text] [Document])
reportErrors = \case
  Errors xs ->
    forM_ xs print >> pure (Errors xs)
  r -> pure r

writeOutput :: Result [Text] [Document] -> IO ()
writeOutput = \case
  Errors _ -> pure ()
  Result xs -> forM_ xs (putStrLn . unlines' . fmap render . documentBlocks)

render :: Block -> Text
render = \case
  Codeblock (Other title) xs ->
    unlines' $ ["```" <> title] ++ xs ++ ["```"]
  Markdown xs ->
    unlines' xs

unlines' :: [Text] -> Text
unlines' = intercalate "\n"

