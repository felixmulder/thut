module Main (main) where

import Prelude hiding (readFile, putStrLn)

import Control.Monad
import Control.Monad (forM_)
import Data.Default.Class (def)
import Data.Either (Either)
import Data.Foldable (foldMap)
import Data.List (zip)
import Data.Text (Text, stripEnd, isPrefixOf, drop, intercalate)
import Data.Text.IO (readFile, putStrLn)
import Data.Traversable (traverse)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Thut.Parser (parseDocument)
import Thut.Types (Block(..), CodeblockType(..), Document(..), Result(..))
import Thut.Interpreter (interpret)
import Thut.Render (renderBlock)

main :: IO ()
main = do
  filePaths <- getArgs
  fileContents <- traverse readFile filePaths

  foldMap (listify <=< interpret def) (uncurry parseDocument <$> zip filePaths fileContents)
    >>= reportErrors
    >>= writeOutput
    >>= exitCode

listify :: Applicative f => Result a b -> f (Result [a] [b])
listify = pure . \case
  Result a -> Result [a]
  Errors e -> Errors [e]

reportErrors :: Result [Document] [Document] -> IO (Result [Document] [Document])
reportErrors = \case
  r@(Errors docs) -> do
    forM_ docs (putStrLn . intercalate "\n" .  fmap renderBlock . documentBlocks)
    pure r
  r -> pure r

exitCode :: Result [Document] [Document] -> IO ()
exitCode = \case
  Result _ -> pure ()
  Errors _ -> exitFailure

writeOutput :: Result [Document] [Document] -> IO (Result [Document] [Document])
writeOutput = \case
  r@(Result xs) -> do
    forM_ xs (putStrLn . intercalate "\n" . fmap renderBlock . documentBlocks)
    pure r
  r -> pure r
