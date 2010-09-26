import Array
import List
import Data.Ord

calculate m a 1 = 1
calculate m a x = let next = case (even x) of
                               True  -> div x 2
                               False -> 3*x +1
                  in case next <= m of
                      True  -> a ! next
                      False -> 1 + calculate m a next

g m a [] = a
g m a (x:xs) = let v = calculate m a x
               in g m ((//) a [(x,v)]) xs


max_seq n = maxArray $ g n a l
  where a = array (1,n) [(i,0) | i <- l]
        l = [1..n]
        


maxArray a = let (l1,l2) = unzip $ assocs a
                 m       = maximum l2
                 pos_m   = case elemIndex m l2 of
                            Nothing -> 0
                            Just n  -> n
             in l1 !! pos_m




calc 1 = 1
calc x = case (even x) of
          True  -> 1 + calc (div x 2)
          False -> 1 + calc (3*x +1)


h = maximumBy (comparing snd) [(x,calc x)  | x <- filter odd [500000..1000000]]
