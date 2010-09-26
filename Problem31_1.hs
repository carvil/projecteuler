import Data.Array

change :: Int -> Int
change n = memo ! (n,8)
  where memo :: Array (Int,Int) Int
        memo = array bounds [(i, uncurry f i) | i <- range bounds]
        bounds = ((0,1),(n,8))
        f :: Int -> Int -> Int
        f 0 _ = 1
        f i 1 = 1
        f i p = a + b
          where a | i-p' < 0 = 0
                  | otherwise = memo ! ((i-p'), p)
                b = memo ! (i, (p-1))
                p' = pence ! p
        pence :: Array Int Int
        pence = listArray (1,8) [1, 2, 5, 10, 20, 50, 100, 200]

main = do
  print $ change 200

