module Problem.P007Spec (main, spec) where

import Test.Hspec
import Problem.P007

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 13 when n = 6" $ do
      solve 6 `shouldBe` 13
    it "returns 1987 when n = 300" $ do
      solve 300 `shouldBe` 1987
