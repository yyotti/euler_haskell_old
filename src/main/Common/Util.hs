module Common.Util where

digits :: Integral a => a -> [Int]
digits n | n < 0 = []
         | otherwise = digits' n []
  where digits' k ls | k < 10 = fromIntegral k : ls
                     | otherwise = digits' (k `div` 10) $ fromIntegral (k `mod` 10) : ls
