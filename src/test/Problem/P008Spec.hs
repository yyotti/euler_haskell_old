module Problem.P008Spec (main, spec) where

import Test.Hspec
import Problem.P008 hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "digits" $ do
    it "returns [] when n = -1" $ do
      digits (-1 :: Integer) `shouldBe` []
    it "returns [0] when n = 0" $ do
      digits 0 `shouldBe` [0]
    it "returns [1] when n = 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1,0] when n = 10" $ do
      digits 10 `shouldBe` [1, 0]
    it "returns [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0] when n = 123456789012345678901234567890" $ do
      digits 123456789012345678901234567890 `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0]

  describe "findLargestProduct" $ do
    it "returns 0 when (k, ns) = (1, [])" $ do
      findLargestProduct 1 [] `shouldBe` 0
    it "returns 1 when (k, ns) = (1, [1])" $ do
      findLargestProduct 1 [1] `shouldBe` 1
    it "returns 3024 when (k, ns) = (6, [1,2,3,7,8,9])" $ do
      findLargestProduct 6 [1, 2, 3, 7, 8, 9] `shouldBe` 3024

  describe "solve" $ do
    it "returns 3024 when (k, n) = (5, 123789)" $ do
      solve 5 123789 `shouldBe` 3024
    it "returns 23514624000 when (k, n) = (13, <省略>)" $ do
      let num = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450
      solve 13 num `shouldBe` 23514624000
