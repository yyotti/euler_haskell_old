module Common.ArithmeticSpec (main, spec) where

import Test.Hspec
import Common.Arithmetic

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fib" $ do
    it "first 10 terms are [1,1,2,3,5,8,13,21,34,55]" $ do
      take 10 fib `shouldBe` [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
