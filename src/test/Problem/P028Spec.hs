module Problem.P028Spec (main, spec) where

import Test.Hspec
import Problem.P028

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "diagonals" $ do
    it "returns [] when n < 1" $ do
      diagonals 0 `shouldBe` []
    it "returns [] when n is even (1)" $ do
      diagonals 2 `shouldBe` []
    it "returns [] when n is even (2)" $ do
      diagonals 6 `shouldBe` []
    it "returns [1,1,1,1] when n = 1" $ do
      diagonals 1 `shouldBe` [1, 1, 1, 1]
    it "returns [3,5,7,9] when n = 3" $ do
      diagonals 3 `shouldBe` [3, 5, 7, 9]
    it "returns [13,17,21,25] when n = 5" $ do
      diagonals 5 `shouldBe` [13, 17, 21, 25]
    it "returns [31,37,43,49] when n = 7" $ do
      diagonals 7 `shouldBe` [31, 37, 43, 49]
    it "returns [57,65,73,81] when n = 9" $ do
      diagonals 9 `shouldBe` [57, 65, 73, 81]



  describe "solve" $ do
    it "returns 1 when n = 1" $ do
      solve 1 `shouldBe` 1
    it "returns 25 when n = 3" $ do
      solve 3 `shouldBe` 25
    it "returns 101 when n = 5" $ do
      solve 5 `shouldBe` 101
    it "returns 0 when n is even (1)" $ do
      solve 2 `shouldBe` 0
    it "returns 0 when n is even (2)" $ do
      solve 6 `shouldBe` 0
