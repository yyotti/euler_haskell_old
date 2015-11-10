module Problem.P006Spec (main, spec) where

import Test.Hspec
import Problem.P006 hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 2640 when n = 10" $ do
      solve 10 `shouldBe` 2640
    it "returns 25164150 when n = 100" $ do
      solve 100 `shouldBe` 25164150
