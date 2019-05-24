module Thut.FileTests where

import           Prelude

import           Control.Monad ((<=<), forM_)
import           Data.Algorithm.Diff (Diff(..), getDiff)
import           Data.Function (on)
import           Data.Default.Class (def)
import           Data.List (sortBy)
import qualified Data.List as List
import           Data.String (IsString)
import           Data.Text (Text, pack, splitOn, unpack)
import qualified Data.Text.IO as Text
import           System.Directory (listDirectory)
import           Test.Tasty.Hspec
import           Thut

spec_positive_files :: Spec
spec_positive_files = do
  it "have the expected output for" $ do
    actuals <- evalFiles "./corpus/positive/input/"
    expecteds <- readFiles "./corpus/positive/output/"
    fileName . evaluatedPath <$> actuals `shouldBe` fileName . fst <$> expecteds

    let
      matched = zip actuals (snd <$> expecteds)
      testEquals doc expected =
        if documentHasErrors doc then expectationFailure $
          "Errors in file \"" <> evaluatedPath doc <> "\", expected none, got:\n\x1b[0m" <> unpack (renderDocument doc)
        else
          renderDocument doc `shouldBeDiff` expected

    forM_ matched (uncurry testEquals <=< printFile)
    putStr "\n      "

spec_negative_files :: Spec
spec_negative_files = do
  it "have the expected output for" $ do
    actuals <- evalFiles "./corpus/negative/input/"
    expecteds <- readFiles "./corpus/negative/output/"
    fileName . evaluatedPath <$> actuals `shouldBe` fileName . fst <$> expecteds

    let
      matched = zip actuals (snd <$> expecteds)
      testEquals doc expected =
        if not $ documentHasErrors doc then expectationFailure $
          "No errors in file \"" <> evaluatedPath doc <> "\", got:\n\x1b[0m" <> unpack (renderDocument doc)
        else
          renderDocument' testConfig doc `shouldBeDiff` expected

    forM_ matched (uncurry testEquals <=< printFile)
    putStr "\n      "

printFile :: (EvaluatedDocument, a) -> IO (EvaluatedDocument, a)
printFile x@(EvaluatedDocument{..}, _) = putStr ("\n        " <> evaluatedPath) >> pure x

shouldBeDiff :: Text -> Text -> Expectation
shouldBeDiff actual expected =
  let
    diff = getDiff (lines . unpack $ expected) (lines . unpack $ actual)
  in
    if allBoth diff then
      pure ()
    else
      expectationFailure $ "Expected equal, but got diff:\n" <> reset <>
        diffColor diff

allBoth :: [Diff a] -> Bool
allBoth (Both _ _ : xs) = allBoth xs
allBoth [] = True
allBoth _ = False

diffColor :: [Diff String] -> String
diffColor = unlines . fmap addSign
  where addSign (Both _ s) = "   " ++ s
        addSign (First  s) = red "---" ++ s ++ reset
        addSign (Second s) = green "+++" ++ s ++ reset

green :: (Semigroup a, IsString a) => a -> a
green = (<>) "\x1b[32m"

red :: (Semigroup a, IsString a) => a -> a
red = (<>) "\x1b[31m"

reset :: IsString a => a
reset = "\x1b[0m"

testConfig :: InterpreterConfig
testConfig = def
  { configUseColor = False
  , configSymbols = Plain
  }

readFiles :: FilePath -> IO [(FilePath, Text)]
readFiles fp = do
  files <- fmap (fp <>) <$> listDirectory fp
  contents <- mapM Text.readFile files
  pure . sortBy (compare `on` fst) $ zip files contents

evalFiles :: FilePath -> IO [EvaluatedDocument]
evalFiles fp = do
  files <- fmap (fp <>) <$> listDirectory fp
  evaled <- mapM (evalFile' testConfig) files
  pure $ sortBy (compare `on` evaluatedPath) evaled

fileName :: FilePath -> Text
fileName = List.last . splitOn "/" . pack

