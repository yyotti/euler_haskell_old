module Problem.P003 where
import Data.Time

{-
- Largest prime factor
- https://projecteuler.net/problem=3
-}

{-
- [回答方針]
-
- [結果]
-
-}
solve :: Integer -> Integer
solve _ = undefined

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve 600851475143
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
