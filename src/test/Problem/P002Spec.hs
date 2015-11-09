module Problem.P002Spec (main, spec) where

import Test.Hspec
import Problem.P002 hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fib" $ do
    it "returns 0 when n = 0" $ do
      fib 0 `shouldBe` 0
    it "returns 1 when n = 1" $ do
      fib 1 `shouldBe` 1
    it "returns 2 when n = 2" $ do
      fib 2 `shouldBe` 2
    it "returns 3 when n = 3" $ do
      fib 3 `shouldBe` 3
    it "returns 5 when n = 4" $ do
      fib 4 `shouldBe` 5
    it "returns 8 when n = 5" $ do
      fib 5 `shouldBe` 8
    it "returns 13 when n = 6" $ do
      fib 6 `shouldBe` 13

  describe "solve" $ do
    it "returns 44 when n = 100" $ do
      solve 100 `shouldBe` 44
    it "returns 4613732 when n = 4000000" $ do
      solve 4000000 `shouldBe` 4613732
