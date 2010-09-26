import NumberTheory.Sieve.ONeill
import List
import Char


myElem (x:xs) n 
  | n == x = True
  | n > x  = myElem xs n
  | n < x  = False


g :: Int -> Bool
g n = foldr (&&) True (truncatable (show n))


truncatable :: [Char] -> [Bool]
truncatable n = map (myElem primes) (map h (delete "" (nub (inits n ++ tails n))))
  where h n = read n :: Int


main = print $ sum (take 11 $ filter g (drop 4 primes))
