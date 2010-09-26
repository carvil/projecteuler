import Bits
import Char
import Numeric.Probability.Example.Collection

xor_int :: Int -> Int -> Int
xor_int a b = a `xor` b

get_key l = map (chr . xor_int 32) l

isolate l = 
  let size = length l
      tmp  = f (size-1)
      l1   = [ (!!) l (i-1) | i <- [ 3*j+1 | j <- [0..tmp] ]]
      l2   = [ (!!) l (i-1) | i <- [ 3*j-1 | j <- [1..tmp] ]]
      l3   = [ (!!) l (i-1) | i <- [ 3*j   | j <- [1..tmp] ]]
  in (select1 l1, select1 l2,select1 l3)
 where f :: Int -> Int
       f n = fromInteger $ round $ fromIntegral n/3


main = do y <- readFile "cipher1.txt"
          l <- parseList y
          let k = decode [103,111,100] l
          print k
  where
    parseList :: String -> IO [Int]
    parseList = readIO


decode key text =
   let l = take (length text) (cycle key)
       k = xor_list text l
   in sum k
  where
    xor_list [] [] = []
    xor_list (x:xs) (y:ys) = (x `xor_int` y) : xor_list xs ys

--(71,0.174563591022444)
--(79,0.21250000000000013)
--(68,0.19250000000000012)
