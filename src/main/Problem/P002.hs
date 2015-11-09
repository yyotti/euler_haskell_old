module Problem.P002 where
import Data.Time

{-
- Even Fibonacci numbers
- https://projecteuler.net/problem=2
-}

solve :: Int -> Integer
solve _ = undefined

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve 4000000
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
