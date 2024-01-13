repeated f x n | n == 0 = x
               | otherwise = repeated f (f x) (n - 1)