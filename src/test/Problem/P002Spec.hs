module Problem.P002Spec (main, spec) where

import Test.Hspec
import Problem.P002 hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fib" $ do
    it "first 10 terms are [1,2,3,5,8,13,21,34,55,89]" $ do
      take 10 fib `shouldBe` [1, 2, 3, 5, 8, 13, 21, 34, 55, 89]

  describe "solve" $ do
    it "returns 44 when n = 100" $ do
      solve 100 `shouldBe` 44
    it "returns 4613732 when n = 4000000" $ do
      solve 4000000 `shouldBe` 4613732
