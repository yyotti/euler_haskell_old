module Problem.P015 where
import Common.Arithmetic

{-
- Lattice paths
- https://projecteuler.net/problem=15
-}

{-
- [方針1]
- 格子の左上から右下まで、引き返しなしで行くには、格子の1辺のサイズをnとすると、
- どのルートも必ず左方向にn、右方向にnの2nステップ進まなければならない。
- 左に進む場合を.、右に進む場合を^で表すと、各ルートは
-   ..^.^^..^^. ・・・
- のように表現できる。
- つまり、2n個あるステップのうち、どのn個を.(左移動)とするかを選択する個数が
- そのままルートの数になるので、2nCnで得られる。
-
- nCrの計算自体は再帰的にも計算できるが、
-   nCr = nPr/r!
- の方が速いと思う。
-
- [結果]
- 137846528820
- time:0.000078s
-}

permutation :: Integral a => a -> a -> a
permutation _ 0 = 1
permutation n 1 = n
permutation n r | n < r = 1
                | otherwise = n * permutation (n - 1) (r - 1)

combination :: Integral a => a -> a -> a
combination n r = div (permutation n r) (fact r)

solve :: Integral a => a -> Integer
solve n = toInteger $ combination (2 * n) n
