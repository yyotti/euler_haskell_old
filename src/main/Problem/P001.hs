module Problem.P001 where
import Data.Time

{-
- Multiples of 3 and 5
- https://projecteuler.net/problem=1
-
- [方針1] -> time:0.001429s
- 普通にfilterして和をとる
-
- [方針2] -> time:0.000161s
- 3の倍数である自然数の数列をa(n)、5の倍数である自然数の数列をb(n)とすると、
-   a(n) = 3n
-   b(n) = 5n
- である。これらの第n項までの和をそれぞれ Sa(n)、Sb(n) とすれば、
-   Sa(n) = 3n(n + 1)/2
-   Sb(n) = 5n(n + 1)/2
- である。nは項数なので、N未満の自然数の中にそれぞれの倍数がいくつあるかを
- あらかじめ計算しておく必要がある。3の倍数の個数をi、5の倍数の個数をjとすると、
-   i = <(N - 1) / 3>
-   j = <(N - 1) / 5>
-   (ただし<a>はa以下の最大の整数を表す)
- である。
- よって、Sa(i) + Sb(j)を計算すれば3の倍数と5の倍数の和になるが、これには
- 15の倍数の和が2回足されているので、1回分を引かなければならない。15の倍数の
- 和を Sc(n) とすれば、
-   Sc(n) = 15n(n + 1)/2
- であり、N未満の自然数の中にある15の倍数の個数kは
-   k = <(N - 1) / 15>
- であるから、求める和Sは
-   S = Sa(i) + Sb(j) - Sc(k)
- となる。
-
- ここで、Sa(n),Sb(n),Sc(n)のパラメータnを、項数ではなく「n未満の項」に変更する。
- また、Sa(n),Sb(n),Sc(n)をまとめて S'(a, n) とする。
- このときS'(a, n)は
-   S'(a, n) = a * m * (m + 1) / 2 (ただし m = <(n - 1) / a>)
- となる。よってSは
-   S = S'(3, n) + S'(5, n) - S(15, n)
- でよい。
-}

sum' :: Int -> Int -> Int
sum' a n = a * m * (m + 1) `div` 2
  where m = (n - 1) `div` a

solve :: Int -> Int
solve n = sum' 3 n + sum' 5 n - sum' 15 n

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve 1000
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
