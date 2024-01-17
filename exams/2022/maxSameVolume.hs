glasses :: [(Integer, Integer)]
glasses = [(5, 12), (2, 16), (4, 4), (3, 36), (9, 4), (6, 9)]

volume :: (Floating a1, Integral a2, Integral a3) => (a2, a3) -> a1
volume (r, h) = 3.14 * (fromIntegral r ** 2) * fromIntegral h

maxSameVolume :: (Integral a1, Integral a2) => [(a1, a2)] -> [(a1, a2)]
maxSameVolume lst = map snd (filter (\(volume, _) -> volume == fst (maximum res)) res)
  where
    res = map (\x -> (volume x, x)) lst
