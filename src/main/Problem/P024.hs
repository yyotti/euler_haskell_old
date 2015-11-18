module Problem.P024 where
import Data.List hiding (permutations)

{-
- Lexicographic permutations
- https://projecteuler.net/problem=24
-}

{-
- [方針1]
- permutationsを使えば簡単と思ったら、Haskellのpermutationsは今回の
- 希望に沿った並び方をしてくれないらしいので、自分で作る。
-
- [結果]
- 2783915460
- time:0.51052s
-}

permutations :: Eq a => [a] -> [[a]]
permutations [] = []
permutations [x] = [[x]]
permutations ls = concatMap (\ x -> map (x:) $ permutations $ delete x ls) ls

solve :: (Show a, Integral a) => [a] -> Int -> Integer
solve ns i = toInteger $ toNum $ (permutations ns) !! (i - 1)
  where toNum = foldl' (\ n k -> n * 10 + k) 0
