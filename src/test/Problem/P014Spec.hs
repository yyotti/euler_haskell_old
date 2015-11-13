module Problem.P014Spec (main, spec) where

import Test.Hspec
import Problem.P014
import Data.Map hiding (foldl')
import Data.List (foldl')

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "chainLength" $ do
    let m = insert 1 1 empty
    let addSeq ls = foldl' (\ mm (k, v) -> insert k v mm) m $ zip ls $ reverse [2..(length ls + 1)]
    it "returns 1 when n = 1" $ do
      chainLength 1 m `shouldBe` (1, m)
    it "returns 2 when n = 2" $ do
      chainLength 2 m `shouldBe` (2, insert 2 2 m)
    it "returns 8 when n = 3" $ do
      let m' = insert 3 2 m
      chainLength 3 m' `shouldBe` (2, m')
    it "returns 8 when n = 3" $ do
      let m' = addSeq [3, 10, 5, 16, 8, 4, 2]
      chainLength 3 m `shouldBe` (8, m')
    it "returns 3 when n = 4" $ do
      let m' = addSeq [4, 2]
      chainLength 4 m `shouldBe` (3, m')
    it "returns 6 when n = 5" $ do
      let m' = addSeq [5, 16, 8, 4, 2]
      chainLength 5 m `shouldBe` (6, m')
    it "returns 9 when n = 6" $ do
      let m' = addSeq [6, 3, 10, 5, 16, 8, 4, 2]
      chainLength 6 m `shouldBe` (9, m')
    it "returns 17 when n = 7" $ do
      let m' = addSeq [7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2]
      chainLength 7 m `shouldBe` (17, m')
    it "returns 17 when n = 7" $ do
      let m' = addSeq [7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2]
      let m'' = addSeq [13, 40, 20, 10, 5, 16, 8, 4, 2]
      chainLength 7 m'' `shouldBe` (17, m')
    it "returns 4 when n = 8" $ do
      let m' = addSeq [8, 4, 2]
      chainLength 8 m `shouldBe` (4, m')
    it "returns 20 when n = 9" $ do
      let m' = addSeq [9, 28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2]
      chainLength 9 m `shouldBe` (20, m')
    it "returns 7 when n = 10" $ do
      let m' = addSeq [10, 5, 16, 8, 4, 2]
      chainLength 10 m `shouldBe` (7, m')

  describe "solve" $ do
    it "returns 27 when n = 30" $ do
      solve 30 `shouldBe` 27
    it "returns 871 when n = 1000" $ do
      solve 1000 `shouldBe` 871
