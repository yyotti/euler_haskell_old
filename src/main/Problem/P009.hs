module Problem.P009 where
import Data.Time

{-
- Special Pythagorean triplet
- https://projecteuler.net/problem=9
-}

{-
- [方針1]
-
- [結果]
-}
solve :: Int -> Int
solve _ = undefined

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
