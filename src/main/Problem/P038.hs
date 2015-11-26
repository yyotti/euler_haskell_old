module Problem.P038 where
import Common.Util
import Control.Monad

{-
- Pandigital multiples
- https://projecteuler.net/problem=38
-}

{-
- [方針1]
- 掛ける数は (1,2,...,n) (n > 1) なので、2以上である。
- d桁の整数に1桁の数字を掛け合わせると、d桁もしくは(d + 1)桁となる。
- 掛けられる数をi、その桁数をdとすると、
-   d = 5の場合
-     i x 1 -> 5桁
-     i x 2 -> 5桁 or 6桁
- となるため、連結積が9桁のパンデジタル数になることはない。
-
-   d = 4の場合
-     i x 1 -> 4桁
-     i x 2 -> 4桁 or 5桁
- であるから、掛けられる数が4桁以下の整数で考えればよい。
-
- [結果]
- 932718654
- time:0.020963s
-}

concatNum :: Integral a => a -> a -> a
-- concatNum a b = a * 10^(e b) + b
-- ↓やりすぎな気はする
concatNum = ap (+) . flip (.) ((10^) . e) . (*)
  where e = length . digits

concatProducts :: Integral a => a -> [a]
concatProducts = ap (scanl concatNum) (flip map [2..] . (*))

solve :: Integer
solve = (toInteger . maximum . filter (isPandigital . (:[])) . map cp) [1..9999]
  where cp = head . dropWhile (< 10^8) . concatProducts
