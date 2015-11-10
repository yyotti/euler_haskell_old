module Problem.P005 where
import Data.Time

{-
- Smallest multiple
- https://projecteuler.net/problem=5
-}

{-
- [方針1]
- m < n とし、mからnまでの全ての数で割り切れる数をNとすると、
- Nはmからnまでの全ての数字の最小公倍数である。
-
- [結果]
- 232792560
- time:0.000186s
-}
solve :: Integer -> Integer -> Integer
solve m n | m > n = 0
          | otherwise = foldr (lcm) 1 [m .. n]

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve 1 20
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
