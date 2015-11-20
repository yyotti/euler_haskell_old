module Problem.P030 where
import Common.Util

{-
- Digit fifth powers
- https://projecteuler.net/problem=30
-}

{-
- [方針1]
- まず範囲を決める。
-
- k桁の整数aのi桁目の数字をa(i) (0 <= a(i) <= 9, 0 <= i <= k-1) とする。
- このときaは
-   a = a(k-1)*10^(k-1) + a(k-2)*10^(k-2) + ... + a(1)*10^1 + a(0)*10^0
- と表せる。
-
- aの各桁をn乗した数の和をS(a, n)とすれば、
-   S(a, n) = a(k-1)^n + a(k-2)^n + ... + a1^n + a0^n
-           = Σ(i = 0 -> k-1) { ai^n }
- である。
- S(n)が最大になるのは全ての桁が9の場合なので、
-   max S(a, n) = Σ(i = 0 -> k-1) { 9^n }
-               = k*9^n
- となる。この桁数をmとすると、m < kの場合は条件を満たすことがない。
- よって、
-   m = log10(max S(a, n))
-     = log10(k*9^n) >= k
- つまり
-   k*9^n >= 10^k
- を満たせばよい。
-
- S(n)の最小値について考えると、1^4は含まないということなので、
- nが1桁では条件を満たす数が存在しない。よって k >= 2 で考えれば
- よい。
-
- [結果]
- 443839
- time:0.745951s
-}

findMax :: Integral a => a -> Int
findMax n = length $ takeWhile (\ i -> i * 9^n >= 10^(i - 1)) [1..]

findDigitPowers :: Integral a => a -> [a]
findDigitPowers n = filter (\ i -> digitSum i == i) [10..10^k - 1]
  where k = findMax n
        digitSum = sum . map (^n) . digits

solve :: Int -> Integer
solve = toInteger . sum . findDigitPowers
