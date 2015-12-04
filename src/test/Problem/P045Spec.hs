module Problem.P045Spec (main, spec) where

import Test.Hspec
import Problem.P045

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 40755 when n = 0" $ do
      solve 0 `shouldBe` 40755
