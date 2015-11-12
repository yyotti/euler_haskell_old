module Problem.P008Spec (main, spec) where

import Test.Hspec
import Problem.P008

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "digits" $ do
    it "returns [] when n = -1" $ do
      digits (-1) `shouldBe` []
    it "returns [0] when n = 0" $ do
      digits 0 `shouldBe` [0]
    it "returns [1] when n = 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1,0] when n = 10" $ do
      digits 10 `shouldBe` [1, 0]
    it "returns [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0] when n = 123456789012345678901234567890" $ do
      digits 123456789012345678901234567890 `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0]

  describe "findLargestProduct" $ do
    it "returns 0 when (k, ns) = (1, [])" $ do
      findLargestProduct 1 [] `shouldBe` 0
    it "returns 1 when (k, ns) = (1, [1])" $ do
      findLargestProduct 1 [1] `shouldBe` 1
    it "returns 3024 when (k, ns) = (6, [1,2,3,7,8,9])" $ do
      findLargestProduct 6 [1, 2, 3, 7, 8, 9] `shouldBe` 3024

  describe "solve" $ do
    it "returns 3024 when (k, n) = (5, 123789)" $ do
      solve 5 123789 `shouldBe` 3024
    it "returns 1959552 when (k, n) = (7, 129999078998769432145)" $ do
      solve 7 129999078998769432145 `shouldBe` 1959552
