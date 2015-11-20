module Problem.P029Spec (main, spec) where

import Test.Hspec
import Problem.P029

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 15 when n = 5" $ do
      solve 5 `shouldBe` 15
