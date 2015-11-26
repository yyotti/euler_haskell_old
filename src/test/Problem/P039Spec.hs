module Problem.P039Spec (main, spec) where

import Test.Hspec
import Problem.P039

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 12 when n = 12" $ do
      solve 12 `shouldBe` 12
