module Problem.P034 where
import Common.Util
import Common.Arithmetic

{-
- Digit fractorials
- https://projecteuler.net/problem=34
-}

{-
- [方針1]
- k桁の整数nについて、各桁を ak,a(k-1),...,a1 とする。
- このとき各桁の階乗和Sは
-   S = (ak)! + (a(k-1))! + ... + (a1)!
- となる。
- 各桁の数字は全て0以上9以下なので、Sの最大値Smaxは
-   Smax = k*9! = 362880k
- である。
-
- k = 6のとき Smax = 2177280 (7桁)
- k = 7のとき Smax = 2540160 (7桁)
- k = 8のとき Smax = 2903040 (7桁)
- となるから、nが8桁になると常に n > S となってしまい、一致することは
- ありえない。よって n <= 2540160 の範囲で探せばよいが、問題に 1! = 1 と
- 2! = 2 は除外するとあるので、 3 <= n <= 2540160 の範囲で探す。
-
- [結果]
- 40730
- time:2.190381s
-}

facts :: [Int]
facts = map fact [0..9]

digitFactSum :: Integral a => a -> Int
digitFactSum = sum . map (facts !!) . digits

solve :: Integer
solve = toInteger $ sum $ filter (\ n -> digitFactSum n == n) nums
  where mx = 7 * fact 9
        nums = [3..mx]
