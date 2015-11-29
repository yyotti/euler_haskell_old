module Problem.P043Spec (main, spec) where

import Test.Hspec
import Problem.P043

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "perms" $ do
    it "returns [[]] when (n,ls) = (0,[1])" $ do
      perms 0 [1] `shouldBe` [[]]
    it "returns [[1]] when (n,ls) = (1,[1])" $ do
      perms 1 [1] `shouldBe` [[1]]
    it "returns [] when (n,ls) = (2,[1])" $ do
      perms 2 [1] `shouldBe` []
    it "returns [[1],[2]] when (n,ls) = (1,[1,2])" $ do
      perms 1 [1, 2] `shouldBe` [[1], [2]]
    it "returns [[1,2],[2,1]] when (n,ls) = (2,[1,2])" $ do
      perms 2 [1, 2] `shouldBe` [[1, 2],[2, 1]]
    it "returns [[1],[2],[3]] when (n,ls) = (1,[1,2,3])" $ do
      perms 1 [1, 2, 3] `shouldBe` [[1], [2], [3]]
    it "returns [[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]] when (n,ls) = (2,[1,2,3])" $ do
      perms 2 [1, 2, 3] `shouldBe` [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]]
    it "returns 504 elements when n ls = (3, [1,2,3,4,5,6,7,8,9]" $ do
      length (perms 3 [1, 2, 3, 4, 5, 6, 7, 8, 9]) `shouldBe` 504

  describe "mkNum" $ do
    it "returns [] when (ps,ls,ds) = ([13,11,7,5,3,2,1],[0,1,7],[2,3,4,5,6,8,9])" $ do
      mkNum [13, 11, 7, 5, 3, 2] [0, 1, 7] [2, 3, 4, 5, 6, 8, 9] `shouldBe` []
    it "returns 4-length list when (ps,ls,ds) = ([13,11,7,5,3,2,1],[2,8,9],[0,1,3,4,5,6,7])" $ do
      length (mkNum [13, 11, 7, 5, 3, 2] [2, 8, 9] [0, 1, 3, 4, 5, 6, 7]) `shouldBe` 4

  describe "solve" $ do
    it "is 16695334890" $ do
      solve `shouldBe` 16695334890
