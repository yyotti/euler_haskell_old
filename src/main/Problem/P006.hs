module Problem.P006 where
import Data.Time

{-
- Sum square difference
- https://projecteuler.net/problem=6
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
  print $ solve 100
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
