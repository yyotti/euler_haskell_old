module Problem.P024Spec (main, spec) where

import Test.Hspec
import Problem.P024

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "permutations" $ do
    it "returns [[0,1,2],[0,2,1],[1,0,2],[1,2,0],[2,0,1],[2,1,0]] when ls = [0,1,2]" $ do
      permutations [0, 1, 2] `shouldBe` [[0, 1, 2], [0, 2, 1], [1, 0, 2], [1, 2, 0], [2, 0, 1], [2, 1, 0]]

  describe "solve" $ do
    it "returns 12 when (ns,i) = ([0,1,2],1)" $ do
      solve [0, 1, 2] 1 `shouldBe` 12
    it "returns 21 when (ns,i) = ([0,1,2],2)" $ do
      solve [0, 1, 2] 2 `shouldBe` 21
    it "returns 102 when (ns,i) = ([0,1,2],3)" $ do
      solve [0, 1, 2] 3 `shouldBe` 102
    it "returns 120 when (ns,i) = ([0,1,2],4)" $ do
      solve [0, 1, 2] 4 `shouldBe` 120
    it "returns 201 when (ns,i) = ([0,1,2],5)" $ do
      solve [0, 1, 2] 5 `shouldBe` 201
    it "returns 210 when (ns,i) = ([0,1,2],6)" $ do
      solve [0, 1, 2] 6 `shouldBe` 210
