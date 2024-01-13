repeated :: (Eq t1, Num t1) => (t2 -> t2) -> t2 -> t1 -> t2
repeated f x n | n == 0 = x
               | otherwise = repeated f (f x) (n - 1)