module Problem.P035Spec (main, spec) where

import Test.Hspec
import Problem.P035

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 13 when n = 100" $ do
      solve 100 `shouldBe` 13
