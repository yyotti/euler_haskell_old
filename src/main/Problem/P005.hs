module Problem.P005 where
import Data.Time

{-
- Smallest multiple
- https://projecteuler.net/problem=5
-}

{-
- [回答方針]
-
- [結果]
-}
solve :: Int -> Int -> Integer
solve _ _ = undefined

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve 1 20
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
