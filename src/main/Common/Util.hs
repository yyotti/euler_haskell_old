module Common.Util where

digits :: Integral a => a -> [a]
digits n | n < 0 = []
         | otherwise = digits' n []
  where digits' k ls | k < 10 = k : ls
                     | otherwise = digits' (k `div` 10) $ (k `mod` 10) : ls
