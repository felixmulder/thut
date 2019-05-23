module Thut.FileTests where

import           Prelude

import           Control.Monad ((<=<), forM_)
import           Data.Algorithm.Diff (Diff(..), getDiff)
import           Data.Function (on)
import           Data.Default.Class (def)
import           Data.List (sortBy)
import           Data.String (IsString)
import           Data.Text (Text, unpack)
import qualified Data.Text.IO as Text
import           System.Directory (listDirectory)
import           Thut (evalText')
import           Thut.Types (InterpreterConfig(..))
import           Test.Tasty.Hspec

readFiles :: FilePath -> IO [(FilePath, Text)]
readFiles fp = do
  files <- listDirectory fp
  contents <- mapM (Text.readFile . (fp <>)) files
  pure . sortBy (compare `on` fst) $ zip files contents

parseFiles :: [(FilePath, Text)] -> IO [(FilePath, Either Text Text)]
parseFiles =
  let
    evaluate fp cont = fmap
      (fp,)
      (evalText' def {configUseColor = False} cont)
  in
    mapM (uncurry evaluate)

evalFiles :: FilePath -> IO [(FilePath, Either Text Text)]
evalFiles = parseFiles <=< readFiles

spec_positive_files :: Spec
spec_positive_files = do
  it "have the expected output for" $ do
    actuals <- evalFiles "./corpus/positive/input/"
    expecteds <- readFiles "./corpus/positive/output/"
    fst <$> actuals `shouldBe` fst <$> expecteds

    let
      matched =
        zipWith (\a b -> (fst a, snd a, snd b)) actuals expecteds
      testEquals = \case
        (fp, Left res, _) -> expectationFailure $
          "Error in file \"" <> fp <> "\", got:\n\x1b[0m" <> unpack res
        (_, Right actual, expected) ->
          actual `shouldBeDiff` expected

    forM_ matched (testEquals <=< printFile)
    putStr "\n      "

spec_negative_files :: Spec
spec_negative_files = do
  it "have the expected output for" $ do
    actuals <- evalFiles "./corpus/negative/input/"
    expecteds <- readFiles "./corpus/negative/output/"
    fst <$> actuals `shouldBe` fst <$> expecteds

    let
      matched =
        zipWith (\a b -> (fst a, snd a, snd b)) actuals expecteds
      testEquals = \case
        (fp, Right res, _) -> expectationFailure $
          "No errors in file \"" <> fp <> "\", got:\n\x1b[0m" <> unpack res
        (_, Left actual, expected) -> do
          actual `shouldBeDiff` expected

    forM_ matched (testEquals <=< printFile)
    putStr "\n      "

printFile :: (FilePath, a, b) -> IO (FilePath, a, b)
printFile x@(fp, _, _) = putStr ("\n        " <> fp) >> pure x

shouldBeDiff :: Text -> Text -> Expectation
shouldBeDiff actual expected =
  let
    diff = getDiff (lines . unpack $ actual) (lines . unpack $ expected)
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
