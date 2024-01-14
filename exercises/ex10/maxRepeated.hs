countMyHead :: Eq a => [a] -> Int
countMyHead lst = length $ takeWhile (\x -> x == head lst) lst

compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress lst = (head lst, n) : compress (drop n lst)
    where n = countMyHead lst

maxRepeated :: Eq a => [a] -> Int
maxRepeated lst = maximum $ map snd $ compress lst