module Problem.P014Spec (main, spec) where

import Test.Hspec
import Problem.P014

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 27 when n = 30" $ do
      solve 30 `shouldBe` 27
    it "returns 871 when n = 1000" $ do
      solve 1000 `shouldBe` 871
