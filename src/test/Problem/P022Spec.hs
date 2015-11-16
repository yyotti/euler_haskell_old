module Problem.P022Spec (main, spec) where

import Test.Hspec
import Problem.P022

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 53 when fpath = \"src/test/resources/p022_1.txt\"" $ do
      solve "src/test/resources/p022_1.txt" `shouldBe` 53
    it "returns 53 when fpath = \"src/test/resources/p022_1.txt\"" $ do
      solve "src/test/resources/p022_2.txt" `shouldBe` 100
