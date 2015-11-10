module Problem.P007 where
import Data.Time

{-
- 10001st prime
- https://projecteuler.net/problem=7
-}

{-
- [回答方針]
-
- [結果]
-}
solve :: Int -> Integer
solve _ = undefined

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve 10001
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
