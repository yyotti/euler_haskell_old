module Problem.P043Spec (main, spec) where

import Test.Hspec
import Problem.P043

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "is 16695334890" $ do
      solve `shouldBe` 16695334890
