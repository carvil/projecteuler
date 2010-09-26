module Main where

--73682

main = print $ changes 200 [1,2,5,10,20,50,100,200]


changes n list
  | list == [] = 0
  | n    == 0  = 1
  | n    <  0  = 0
  | otherwise  = let rh = changes n (tail list)
                     lh = changes (n-(head list)) list
                 in rh + lh
