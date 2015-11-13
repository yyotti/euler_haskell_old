module Problem.P014 where
import Data.Ord
import Data.Map as Map hiding (foldl')
import Data.List hiding (insert)

{-
- Longest Collatz sequence
- https://projecteuler.net/problem=14
-}

{-
- [方針1]
- 問題に書かれている定義に従って処理する。
-
- [結果]
- 837799
- time:28.338644s
-
- [コミット]
- 16b9f53
-}

{-
- [方針2]
- 問題にある数列に着目する。
- 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
- この数列は13から始まり10個の項で構成されるが、途中の20から始めた場合は
- 20 -> 10 -> ...
- と続き8個の項で数列を構成する。
- つまり、ある数から始めて生成された数列の部分列は再利用できる。
-
- よって下記のアルゴリズムで考える。
- まず、数字をキーとするマップを用意する。値は、その数字から始めた場合の
- コラッツ数列の長さとする。マップのキーと値の組を
-   [k -> v]
- で表現する。13の場合は10項の数列なので
-   [13 -> 10]
- である。
- ある数について数列の長さを求める際、まずマップに登録されていないかを
- 調べ、登録されていればその長さを用いる。
- ある整数nについて、
-   (nがマップに登録されている場合)
-     その値を採用する。
-   (nがマップに登録されていない場合)
-     定義に従って次の数字mを計算する。n=mとして再帰的に数列の長さを計算し、
-     その長さ+1をnの値とする。
-
- [結果]
- 837799
- time:8.890178s
-}

chainLength :: Integral a => a -> Map a Int -> (Int, Map a Int)
chainLength n m = case Map.lookup n m of
                       Just c -> (c, m)
                       _ -> let (c, m') = chainLength n' m in (c + 1, insert n (c + 1) m')
  where n' | even n = n `div` 2
           | otherwise = 3 * n + 1

solve :: Integral a => a -> Integer
solve n = toInteger $ fst $ maximumBy (comparing snd) $ toList m
  where m = foldl' (\ m i -> snd $ chainLength i m) (insert 1 1 empty) [2..n]
