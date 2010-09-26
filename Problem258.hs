module Main (g) where
g :: (Num a, Ord a) => a -> a
g k
  | k < 0 = 0 
  | 0 <= k && k <= 1999 = 1
  | k >= 2000 = g (k-2000) + g (k-1999)


h k 
  | k < 0 = 0 
  | 0 <= k && k <= 1999 = 1
  | k >= 2000 = 2 * div k 2000

fibs = 0 : 1 : [ x+y | (x,y) <- zip fibs (tail fibs) ]

goldenRatio = 1.6180339887


f n = let r = (g n) 
      in r `mod` 20092010

main = print $ f (10^18)
