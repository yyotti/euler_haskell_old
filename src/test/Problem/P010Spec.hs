module Problem.P010Spec (main, spec) where

import Test.Hspec
import Problem.P010 hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sieve" $ do
    it "returns [] when n = 1" $ do
      sieve 1 `shouldBe` []
    it "returns [2] when n = 2" $ do
      sieve 2 `shouldBe` [2]
    it "returns [2,3] when n = 3" $ do
      sieve 3 `shouldBe` [2,3]
    it "returns [2,3] when n = 4" $ do
      sieve 4 `shouldBe` [2,3]
    it "returns [2,3,5] when n = 5" $ do
      sieve 5 `shouldBe` [2,3,5]
    it "returns [2,3,5,7,11,13,17,19] when n = 20" $ do
      sieve 20 `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19]

  describe "solve" $ do
    it "returns 17 when n = 10" $ do
      solve 10 `shouldBe` 17
    it "returns 142913828922 when n = 2000000" $ do
      solve 2000000 `shouldBe` 142913828922
