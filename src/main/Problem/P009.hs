module Problem.P009 where
import Data.Time

{-
- Special Pythagorean triplet
- https://projecteuler.net/problem=9
-}

{-
- [方針1]
- cが斜辺、a,bが直角を挟む2つの辺なので、大小関係として
-   a <= b < c
- としてよい。
- また
-   a + b + c = n
- とすると、
-   c = n - a - b
- であるから、
-   b < n - a - b
- より
-   b < (n - a)/2
- であり、
-   a <= b < (n - a)/2
- より
-   a <= n/3
- である。
-
- あとは上記の関係を満たす(a, b, c)について、ピタゴラス数に
- なるものを探し積を求めればよい。
-
- [結果]
- 31875000
- time:0.113426s
-}

isPythagoreanTripret :: (Int, Int, Int) -> Bool
isPythagoreanTripret (a, b, c) = a * a + b * b == c * c

findSpecialPythagoreanTriprets :: Int -> [(Int, Int, Int)]
findSpecialPythagoreanTriprets n  = filter isPythagoreanTripret $ concatMap (\ a -> map (\ b -> (a, b, n - a - b)) [a .. ((n - a) `div` 2)]) [1..(n `div` 3)]

solve :: Int -> Int
solve n = case findSpecialPythagoreanTriprets n of
             [] -> 0
             ((a, b, c):_) -> a * b * c

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve 1000
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
