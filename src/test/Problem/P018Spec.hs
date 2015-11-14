module Problem.P018Spec (main, spec) where

import Test.Hspec
import Problem.P018

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 23 when t = [[3],[7,4],[2,4,6],[8,5,9,3]]" $ do
      let t = [[3], [7, 4], [2, 4, 6], [8, 5, 9, 3]]
      solve t `shouldBe` 23
