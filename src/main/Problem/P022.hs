module Problem.P022 where
import Data.Char
import Data.List as List

{-
- Names scores
- https://projecteuler.net/problem=22
-}

{-
- [方針1]
- 特に工夫はない
-
- [結果]
- 871198282
- time:0.01994s
-}

calcScore :: String -> Int
calcScore = sum . map (\ c -> ord c - ord 'A' + 1)

sumNameScores :: [String] -> Int
sumNameScores ss = fst $ foldl' (\ (t, l) s -> (t + l * calcScore s, l + 1)) (0, 1) ss

splitByComma :: String -> [String]
splitByComma "" = []
splitByComma str = h : splitByComma t
  where (h, t) = case span (/= ',') str of
                      (h', (',':s)) -> (h', s)
                      (h', t') -> (h', t')

readNames fpath = do
  str <- readFile fpath
  return . sort . map (tail . init) . splitByComma . head . lines $ str

solve :: String -> IO Integer
solve fpath = (return . toInteger . sumNameScores) =<< readNames fpath
