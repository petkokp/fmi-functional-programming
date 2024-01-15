removeConsequtive :: Eq a => [a] -> [a]
removeConsequtive [] = []
removeConsequtive (x:xs) = x : removeConsequtive (dropWhile (== x) xs)