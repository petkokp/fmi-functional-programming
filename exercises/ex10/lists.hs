apply :: (t -> a) -> [t] -> [a]
apply f [] = []
apply f (x:xs) = f x : apply f xs

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs)| f x = x : filter f xs
                | otherwise = filter f xs
              
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

length :: Num a1 => [a2] -> a1
length [] = 0
length (x:xs) = 1 + length xs

null :: [a] -> Bool
null [] = True
null (x:xs) = False

elem :: Eq t => t -> [t] -> Bool
elem _ [] = False
elem n (x:xs) | n == x = True
              | otherwise = elem n xs

take :: (Eq t, Num t) => t -> [a] -> [a]
take 0 [] = []
take n [] = []
take n (x:xs) | n == 0 = []
              | otherwise = x : take (n - 1) xs

drop :: (Eq t, Num t) => t -> [a] -> [a]
drop _ [] = []
drop n (x:xs) | n == 1 = xs
              | otherwise = drop (n - 1) xs

zip :: [a] -> [b] -> [(a, b)]
zip _ [] = []
zip [] _ = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

zipWith :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith op _ [] = []
zipWith op [] _ = []
zipWith op (x:xs) (y:ys) = op x y : zipWith op xs ys

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile pred [] = []
takeWhile pred (x:xs) | pred x = x : takeWhile pred xs
                      | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile pred [] = []
dropWhile pred (x:xs) | pred x = xs
                      | otherwise = dropWhile pred xs