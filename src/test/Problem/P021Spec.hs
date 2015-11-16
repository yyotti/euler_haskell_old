module Problem.P021Spec (main, spec) where

import Test.Hspec
import Problem.P021

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 504 when n = 1000" $ do
      solve 1000 `shouldBe` 504
    it "returns 8442 when n = 3000" $ do
      solve 3000 `shouldBe` 8442
