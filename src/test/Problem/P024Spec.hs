module Problem.P024Spec (main, spec) where

import Test.Hspec
import Problem.P024

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "permutation" $ do
    it "returns [0,1,2] when (ls,i) = ([0,1,2],0)" $ do
      permutation [0, 1, 2] 0 `shouldBe` [0, 1, 2]
    it "returns [0,2,1] when (ls,i) = ([0,1,2],1)" $ do
      permutation [0, 1, 2] 1 `shouldBe` [0, 2, 1]
    it "returns [1,0,2] when (ls,i) = ([0,1,2],2)" $ do
      permutation [0, 1, 2] 2 `shouldBe` [1, 0, 2]
    it "returns [1,2,0] when (ls,i) = ([0,1,2],3)" $ do
      permutation [0, 1, 2] 3 `shouldBe` [1, 2, 0]
    it "returns [2,0,1] when (ls,i) = ([0,1,2],4)" $ do
      permutation [0, 1, 2] 4 `shouldBe` [2, 0, 1]
    it "returns [2,1,0] when (ls,i) = ([0,1,2],5)" $ do
      permutation [0, 1, 2] 5 `shouldBe` [2, 1, 0]

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
