countMyHead :: Eq a => [a] -> Int
countMyHead lst = length $ takeWhile (\x -> x == head lst) lst

compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress lst = (head lst, n) : compress (drop n lst)
    where n = countMyHead lst

compress' :: Eq a => [a] -> [(a, Int)]
compress' [] = []
compress' lst = helper (head lst) 1 (tail lst)
  where helper curr count [] = [(curr,count)]
        helper curr count (x:xs)
          | curr == x   = helper curr (count+1) xs
          | otherwise   = (curr,count) : helper x 1 xs