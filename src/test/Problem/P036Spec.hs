module Problem.P036Spec (main, spec) where

import Test.Hspec
import Problem.P036

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 25 when n = 10" $ do
      solve 10 `shouldBe` 25
    it "returns 743 when n = 100" $ do
      solve 25 `shouldBe` 743
