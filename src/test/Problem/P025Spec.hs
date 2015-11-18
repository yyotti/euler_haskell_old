module Problem.P025Spec (main, spec) where

import Test.Hspec
import Problem.P025

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 12 when n = 3" $ do
      solve 3 `shouldBe` 12
    it "returns 16 when n = 4" $ do
      solve 4 `shouldBe` 17
    it "returns 20 when n = 5" $ do
      solve 5 `shouldBe` 21
