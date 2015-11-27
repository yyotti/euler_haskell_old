module Problem.P042Spec (main, spec) where

import Test.Hspec
import Problem.P042

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 1 when fpath = \"src/test/resources/p042_1.txt\"" $ do
      ans <- solve "src/test/resources/p042_1.txt"
      ans `shouldBe` 1
