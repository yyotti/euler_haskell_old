module Problem.P030Spec (main, spec) where

import Test.Hspec
import Problem.P030

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 19316 when n = 4" $ do
      solve 4 `shouldBe` 19316
