findSegment :: (Ord a) => [a] -> [a]
findSegment lst = foldl (\acc x -> if x < last acc then acc ++ [x] else acc) [head lst] (tail lst)

segments :: (Ord a) => [a] -> [[a]]
segments [] = []
segments lst = res : segments (drop (length res - 1) (tail lst)) where res = findSegment lst

fillSegments :: (Num b, Enum b, Ord b) => [b] -> [b]
fillSegments lst = concatMap (\x -> reverse [0 .. head x]) (segments lst)
