import List
import Char
import Control.Parallel (par, pseq)

f :: Integer -> Integer
f n = head $ dropWhile ((/= True) . g') [ n*i | i <- [1..]]
--f n = read $ head $ dropWhile ((/= True) . g) [show (n*i) | i <- [1..]]

g' :: Integer -> Bool
g' n = if n < 10 then g'' n else 
       case g'' (n `mod` 10) of
        True  -> g' (n `div` 10)
        False -> False

g'' :: Integer -> Bool
g'' 0 = True
g'' 1 = True
g'' 2 = True
g'' _ = False

g x =all h x
h y = y=='0' || y=='1' || y=='2'

force :: [a] -> ()
force xs = go xs `pseq` ()
    where go (_:xs) = go xs
          go [] = 1


forceList :: [a] -> ()
forceList (x:xs) = x `pseq` forceList xs
forceList _ = ()

main = print $ sum $ calc1 

calc1 = [div (f n) n | n <- [1..998]]

calc = force left `par` (force right `pseq` (left ++ right))
--h = force left `par` (force right `pseq` (sum left + sum right))
  where left  = [div (f n) n | n <- [1..500]]
        right = [div (f n) n | n <- [501..1000]]

--print $ sum $ [div (f n) n | n <- [1..100]]
