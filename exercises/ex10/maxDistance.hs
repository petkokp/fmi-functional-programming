dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

maxDistance :: [(Double, Double)] -> Double
maxDistance pts = maximum [ dist p1 p2 | p1<-pts, p2<-pts ]

maxDistance' :: [(Double, Double)] -> Double
maxDistance' [] = 0
maxDistance' (x:xs) = max (helper x xs) (maxDistance' xs)
  where helper p pts = maximum [ dist p p2 | p2<-pts ]