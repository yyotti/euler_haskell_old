module Problem.P012Spec (main, spec) where

import Test.Hspec
import Problem.P012

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 28 when n = 5" $ do
      solve 5 `shouldBe` 28
    it "returns 120 when n = 15" $ do
      solve 15 `shouldBe` 120
