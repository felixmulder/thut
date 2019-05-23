module Thut.FileTests where

import           Prelude

import           Control.Monad ((<=<), forM_)
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Text (Text, unpack)
import qualified Data.Text.IO as Text
import           System.Directory (listDirectory)
import           Test.Tasty.Hspec
import           Thut

readFiles :: FilePath -> IO [(FilePath, Text)]
readFiles fp = do
  files <- listDirectory fp
  contents <- mapM (Text.readFile . (fp <>)) files
  pure . sortBy (compare `on` fst) $ zip files contents

parseFiles :: [(FilePath, Text)] -> IO [(FilePath, Either Text Text)]
parseFiles = mapM (\(fp, cont) -> (fp,) <$> evalText cont)

evalFiles :: FilePath -> IO [(FilePath, Either Text Text)]
evalFiles = parseFiles <=< readFiles

spec_positive_files :: Spec
spec_positive_files = do
  it "have the expected output" $ do
    actuals <- evalFiles "./corpus/positive/input/"
    expecteds <- readFiles "./corpus/positive/output/"
    fst <$> actuals `shouldBe` fst <$> expecteds

    let
      matched =
        zipWith (\a b -> (fst a, snd a, snd b)) actuals expecteds

    forM_ matched $ \case
      (fp, Left res, _) -> expectationFailure $
        "Error in file \"" <> fp <> "\", got:\n\x1b[0m" <> unpack res
      (_, Right actual, expected) -> do
        actual `shouldBe` expected

spec_negative_files :: Spec
spec_negative_files = do
  it "have the expected output" $ do
    parsed <- evalFiles "./corpus/negative/"
    forM_ parsed $ \case
      (fp, Right res) -> expectationFailure $
        "No errors in file \"" <> fp <> "\", got:\n\x1b[0m" <> unpack res
      (_, _) -> pure () -- TODO: test against expected
