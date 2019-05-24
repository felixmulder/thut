module Main (main) where

import           Prelude

import           Control.Monad (when)
import qualified Data.Text.IO as Text (putStrLn)
import           Data.Traversable (traverse)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           Thut (EvaluatedDocument, documentHasErrors, evalFile, renderDocument)

main :: IO ()
main =
  getArgs
    >>= traverse evalFile
    >>= traverse renderResults
    >>= mapM_ exitCodeOnFailure

renderResults :: EvaluatedDocument -> IO EvaluatedDocument
renderResults doc = do
  Text.putStrLn $ renderDocument doc
  pure doc

exitCodeOnFailure :: EvaluatedDocument -> IO ()
exitCodeOnFailure doc = when (documentHasErrors doc) exitFailure
