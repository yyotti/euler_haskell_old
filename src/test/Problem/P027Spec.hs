module Problem.P027Spec (main, spec) where

import Test.Hspec
import Problem.P027

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 41 when m = 50" $ do
      solve 50 `shouldBe` -235
    it "returns 41 when m = 100" $ do
      solve 100 `shouldBe` -1455
