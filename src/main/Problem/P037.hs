module Problem.P037 where
import Common.Util
import Control.Monad
import Data.List
import Common.Arithmetic

{-
- Truncatable primes
- https://projecteuler.net/problem=37
-}

{-
- [方針1]
- 問題によると11個しかないということなので、素数を順に調べていって
- 切り詰め素数が11個になったところでやめる。
-
- [結果]
- 748317
- time:0.811496s
-
- [コミット]
- f8db587
-}

{-
- [方針2]
- 11個と決め打つのは気持悪いので、全てを生成する方向でやる。
-
- 下記の手順で全ての切り詰め素数を生成することができる。
- 1. 1桁の素数を列挙する。これをPSとする。
- 2. LPS = PS, RPS = PSとする。
- 3. LPSに含まれる数の左に1～9の数字をそれぞれ付加した数を生成し、そこから
-    素数を抽出する。抽出した数をあらためてLPSとする。
- 4. RPSに含まれる数の右に1～9の数字をそれぞれ付加した数を生成し、そこから
-    素数を抽出する。抽出した数をあらためてRTSとする。
- 5. LPS、RTSの少なくとも一方が空になれば、全ての切り詰め可能素数が列挙された。
- 6. LPSとRPSの共通部分をとり、切り詰め可能素数の列に追加する。手順3に戻り、
-    次に大きい桁の切り詰め可能素数を列挙していく手順を繰り返す。
-
- [結果]
- 748317
- time:0.055739s
-}

addLeft :: Integral a => a -> a -> a
-- addLeft n d = n + d * (e n)
-- ↑をポイントフリーで書くと↓になるが、やりぎすに思えてしょうがない
addLeft = ap ((.) . (+)) ((*) . e)
  where e = (10^) . length . digits

addRight :: Integral a => a -> a -> a
addRight = (+) . (10 *)

solve :: Integer
solve = toInteger $ sum $ tps ps ps
  where ps = takeWhile (< 10) primes
        tps [] _ = []
        tps _ [] = []
        tps lps rps = intersect nextL nextR ++ tps nextL nextR
          where nextL = filter isPrime $ concatMap (flip map [1..9] . addLeft) lps
                nextR = filter isPrime $ concatMap (flip map [1..9] . addRight) rps
