module Problem.P018Spec (main, spec) where

import Test.Hspec
import Problem.P018

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pairs" $ do
    it "returns [] when ls = []" $ do
      pairs ([] :: [Char]) `shouldBe` []
    it "returns [] when ls = [1]" $ do
      pairs [1] `shouldBe` []
    it "returns [[1,2]] when ls = [1,2]" $ do
      pairs [1, 2] `shouldBe` [[1, 2]]
    it "returns [[1,2],[2,3]] when ls = [1,2,3]" $ do
      pairs [1, 2, 3] `shouldBe` [[1, 2], [2, 3]]
    it "returns [[1,2],[2,3],[3,4]] when ls = [1,2,3,4]" $ do
      pairs [1, 2, 3, 4] `shouldBe` [[1, 2], [2, 3], [3, 4]]

  describe "findMax" $ do
    it "returns 0 when ls = []" $ do
      findMax [] `shouldBe` 0
    it "returns 3 when ls = [[3]]" $ do
      findMax [[3]] `shouldBe` 3
    it "returns 10 when ls = [[7,4],[3]]" $ do
      findMax [[7, 4], [3]] `shouldBe` 10
    it "returns 14 when ls = [[2,4,6],[7,4],[3]]" $ do
      findMax [[2, 4, 6], [7, 4], [3]] `shouldBe` 14
    it "returns 23 when ls = [[8,5,9,3],[2,4,6],[7,4],[3]]" $ do
      findMax [[8, 5, 9, 3], [2, 4, 6], [7, 4], [3]] `shouldBe` 23

  describe "solve" $ do
    it "returns 23 when t = [[3],[7,4],[2,4,6],[8,5,9,3]]" $ do
      let t = [[3], [7, 4], [2, 4, 6], [8, 5, 9, 3]]
      solve t `shouldBe` 23
