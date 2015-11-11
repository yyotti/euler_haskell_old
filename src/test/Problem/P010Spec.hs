module Problem.P010Spec (main, spec) where

import Test.Hspec
import Problem.P010 hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 17 when n = 10" $ do
      solve 10 `shouldBe` 17
    it "returns 142913828922 when n = 2000000" $ do
      solve 2000000 `shouldBe` 142913828922
