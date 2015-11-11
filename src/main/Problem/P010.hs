module Problem.P010 where
import Data.Time

{-
- Summation of primes
- https://projecteuler.net/problem=10
-}

{-
- [方針1]
-
- [結果]
-}
solve :: Int -> Integer
solve _ = undefined

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve 2000000
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
