module Problem.P002Spec (main, spec) where

import Test.Hspec
import Problem.P002

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 44 when n = 100" $ do
      solve 100 `shouldBe` 44
    it "returns 798 when n = 1000" $ do
      solve 1000 `shouldBe` 798
