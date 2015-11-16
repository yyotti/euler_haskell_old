module Problem.P021Spec (main, spec) where

import Test.Hspec
import Problem.P021
import Data.Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sieveLike" $ do
    it "returns [] when n = 0" $ do
      sieveLike 0 `shouldBe` empty
    it "returns [(1,0)] when n = 1" $ do
      sieveLike 1 `shouldBe` fromList [(1, 0)]
    it "returns [(1,0),(2,1)] when n = 2" $ do
      sieveLike 2 `shouldBe` fromList [(1, 0), (2, 1)]
    it "returns [(1,0),(2,1),(3,1)] when n = 3" $ do
      sieveLike 3 `shouldBe` fromList [(1, 0), (2, 1), (3, 1)]
    it "returns [(1,0),(2,1),(3,1),(4,3)] when n = 4" $ do
      sieveLike 4 `shouldBe` fromList [(1, 0), (2, 1), (3, 1), (4, 3)]

  describe "filterAmicableNumbers" $ do
    it "returns [] when (n,m) = (1,empty)" $ do
      filterAmicableNumbers 1 empty `shouldBe` []
    it "returns [] when (n,m) = (10,[(1,0),(2,1),(3,1),(4,3)])" $ do
      let m = fromList [(1, 0), (2, 1), (3, 1), (4, 3)]
      filterAmicableNumbers 10 m `shouldBe` []
    it "returns [220,284] when (n,m) = (1200,[(1,0),(2,1),(220,284),(284,220),(1184,1210),(1210,1184)])" $ do
      let m = fromList [(1, 0), (2, 1), (220, 284), (284, 220), (1184, 1210), (1210, 1184)]
      filterAmicableNumbers 1200 m `shouldBe` [220, 284]
  -- describe "filterAmicableNumbers" $ do
  --   it "returns [] when (n,ls) = (1,[])" $ do
  --     filterAmicableNumbers 1 [] `shouldBe` []
  --   it "returns [] when (n,ls) = (10,[(1,0),(2,1),(3,1),(4,3)])" $ do
  --     filterAmicableNumbers 10 [(1, 0), (2, 1), (3, 1), (4, 3)] `shouldBe` []
  --   it "returns [220,284] when (n,ls) = (1200,[(1,0),(2,1),(220,284),(284,220),(1184,1210),(1210,1184)])" $ do
  --     filterAmicableNumbers 1200 [(1, 0), (2, 1), (220, 284), (284, 220), (1184, 1210), (1210, 1184)] `shouldBe` [220, 284]

  describe "solve" $ do
    it "returns 504 when n = 1000" $ do
      solve 1000 `shouldBe` 504
    it "returns 8442 when n = 3000" $ do
      solve 3000 `shouldBe` 8442
