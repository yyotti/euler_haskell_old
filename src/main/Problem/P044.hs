module Problem.P044 where
import Control.Monad
import Data.List
import Data.Maybe

{-
- Pentagon numbers
- https://projecteuler.net/problem=44
-}

{-
- [方針1]
- 五角数の列を小さい方から順に調べ、最初に見つけた(j,k)を採用する。
-
- [結果]
- 5482660
- time:42.819109s
-}

pentagonals :: Integral a => [a]
pentagonals = 1 : zipWith (+) pentagonals [4,7..]

isPentagonNumber :: Integral a => a -> Bool
isPentagonNumber = ap (==) (head . flip dropWhile pentagonals . (>))

solve :: Integer
solve = (toInteger . minimum . fromJust . find ((> 0) . length) . map pentagonalPairs) [1..]
  where pentagonalPairs n = map (\ v -> h - v) $ filter (\ v -> isPentagonNumber (h + v) && isPentagonNumber (h - v)) ts
          where (h:ts) = (reverse . take n) pentagonals
