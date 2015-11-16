module Problem.P018Spec (main, spec) where

import Test.Hspec
import Problem.P018

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "findRoutes" $ do
    it "returns [] when ls = []" $ do
      findRoutes [] `shouldBe` []
    it "returns [[1,2,3]] when ls = [[1,2,3]]" $ do
      findRoutes [[1, 2, 3]] `shouldBe` [[1, 2, 3]]
    it "returns [[3,7][3,4]] when ls = [[3],[7,4]]" $ do
      findRoutes [[3], [7, 4]] `shouldBe` [[3, 7], [3, 4]]
    it "returns [[3,7,2],[3,7,4],[3,4,4],[3,4,6]] when ls = [[3],[7,4],[2,4,6]]" $ do
      findRoutes [[3], [7, 4], [2, 4, 6]] `shouldBe` [[3, 7, 2], [3, 7, 4], [3, 4, 4], [3, 4, 6]]
    it "returns [[3,7,2,8],[3,7,2,5],[3,7,4,5],[3,7,4,9],[3,4,4,5],[3,4,4,9],[3,4,6,9],[3,4,6,3]] when ls = [[3],[7,4],[2,4,6],[8,5,9,3]]" $ do
      findRoutes [[3], [7, 4], [2, 4, 6], [8, 5, 9, 3]] `shouldBe` [[3, 7, 2, 8], [3, 7, 2, 5], [3, 7, 4, 5], [3, 7, 4, 9], [3, 4, 4, 5], [3, 4, 4, 9], [3, 4, 6, 9], [3, 4, 6, 3]]

  describe "solve" $ do
    it "returns 23 when t = [[3],[7,4],[2,4,6],[8,5,9,3]]" $ do
      let t = [[3], [7, 4], [2, 4, 6], [8, 5, 9, 3]]
      solve t `shouldBe` 23
