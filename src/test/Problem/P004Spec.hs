module Problem.P004Spec (main, spec) where

import Test.Hspec
import Problem.P004 hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 9009 when n = 2" $ do
      solve 2 `shouldBe` 9009
    it "returns 906609 when n = 3" $ do
      solve 3 `shouldBe` 906609
