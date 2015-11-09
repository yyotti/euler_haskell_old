module Problem.P004 where
import Data.Time

{-
- Largest palindrome product
- https://projecteuler.net/problem=4
-}

{-
- [方針1]
-
-
- [結果]
-}
solve :: Int -> Integer
solve _ = undefined

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve 3
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
