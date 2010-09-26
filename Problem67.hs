import Examples


total67 = head . foldr1 f 
  where f x1 x2 = let l = zipWith max (init x2) (tail x2)
                  in zipWith (+) x1 l
