module Problem.P043Spec (main, spec) where

import Test.Hspec
import Problem.P043

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sliding" $ do
    it "returns [] when (n,ls) = (2,[])" $ do
      sliding 2 "" `shouldBe` []
    it "returns [[1]] when (n,ls) = (2,[1])" $ do
      sliding 2 [1] `shouldBe` [[1]]
    it "returns [[1],[2]] when (n,ls) = (1,[1,2])" $ do
      sliding 1 [1, 2] `shouldBe` [[1], [2]]
    it "returns [[1,2]] when (n,ls) = (2,[1,2])" $ do
      sliding 2 [1, 2] `shouldBe` [[1, 2]]
    it "returns [[1,2],[2,3]] when (n,ls) = (2,[1,2,3])" $ do
      sliding 2 [1, 2, 3] `shouldBe` [[1, 2], [2, 3]]
    it "returns [[1,2,3],[2,3,4],[3,4,5]] when (n,ls) = (3,[1,2,3,4,5])" $ do
      sliding 3 [1, 2, 3, 4, 5] `shouldBe` [[1, 2, 3], [2, 3, 4], [3, 4, 5]]

  describe "slidingNums" $ do
    it "returns [1] when (n,ls) = (2,[1])" $ do
      slidingNums 2 [1] `shouldBe` [1]
    it "returns [1,2] when (n,ls) = (1,[1,2])" $ do
      slidingNums 1 [1, 2] `shouldBe` [1, 2]
    it "returns [12] when (n,ls) = (2,[1,2])" $ do
      slidingNums 2 [1, 2] `shouldBe` [12]
    it "returns [12,23] when (n,ls) = (2,[1,2,3])" $ do
      slidingNums 2 [1, 2, 3] `shouldBe` [12, 23]
    it "returns [123,234,345] when (n,ls) = (3,[1,2,3,4,5])" $ do
      slidingNums 3 [1, 2, 3, 4, 5] `shouldBe` [123, 234, 345]

  describe "isDivisiblePandigital" $ do
    it "returns True when ls = [1,4,0,6,3,5,7,2,8,9]" $ do
      isDivisiblePandigital [1, 4, 0, 6, 3, 5, 7, 2, 8, 9] `shouldBe` True
    it "returns False when ls = [1,4,0,6,3,5,7,2,8,9]" $ do
      isDivisiblePandigital [1, 4, 0, 6, 3, 5, 7, 2, 9, 8] `shouldBe` False

  describe "solve" $ do
    it "is 16695334890" $ do
      solve `shouldBe` 16695334890
