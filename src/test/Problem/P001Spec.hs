module Problem.P001Spec (main, spec) where

import Test.Hspec
import Problem.P001 hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 23 when max = 10" $ do
      solve 10 `shouldBe` 23
    it "returns 233168 when max = 1000" $ do
      solve 1000 `shouldBe` 233168
