module Problem.P033 where
import Common.Util
import Data.List

{-
- Digit cancelling fractions
- https://projecteuler.net/problem=33
-}

{-
- [方針1]
- 分子・分母がともに2桁になるものを探すので、分子・分母ともに10以上100未満で
- 探すことにする。
- ただし、30/50のような形、つまり分母・分子ともに10の倍数になっているものは
- 自明な例として除く。また1より小さい数なので、分子が分母より小さくなければ
- ならない。
-
- [結果]
- 100
- time:0.002376s
-}

type Frac = (Int, Int)

lct :: Frac -> Frac
lct (n, d) = (n `div` g, d `div` g)
  where g = gcd n d

incorrectLct :: Frac -> Frac
incorrectLct (n, d) | n < 10 || n > 99 || d < 10 || n > 99 = (n, d)
incorrectLct (n, d) = cancel (n1, n2) (d1, d2)
  where [n1, n2] = digits n
        [d1, d2] = digits d
        cancel (n1, n2) (d1, d2) | n1 == d1 = (n2, d2)
                                 | n1 == d2 = (n2, d1)
                                 | n2 == d1 = (n1, d2)
                                 | n2 == d2 = (n1, d1)
                                 | otherwise = (n, d)

fractions :: [Frac]
fractions = filter (\ (n, d) -> n `mod` 10 /= 0 || d `mod` 10 /= 0) pairs
  where pairs = concatMap (\ d -> map (\ n -> (n, d)) [10..d-1]) [11..99]

isDcf :: Frac -> Bool
isDcf f = f /= f' && lct f == lct f'
  where f' = incorrectLct f

solve :: Integer
solve = toInteger $ snd $ lct $ foldl' (\ (n1, d1) (n2, d2) -> (n1 * n2, d1 * d2)) (1, 1) $ filter isDcf fractions
