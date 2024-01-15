maximumBy :: Foldable t1 => (t2 -> t2 -> Bool) -> t1 t2 -> t2
maximumBy cmp = foldr1 (\curr res -> if res `cmp` curr then res else curr)