module Problem.P006 where
import Data.Time

{-
- Sum square difference
- https://projecteuler.net/problem=6
-}

{-
- [方針1]
- 問題に書かれていることをそのままやる。
-
- [結果]
- 25164150
- time:0.000244s
-}

solve :: Integer -> Integer
solve n = sumB^(2 :: Int) - sumA
  where sumA = sum $ map (^(2 :: Int)) [1..n]
        sumB = sum [1..n]

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve 100
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
