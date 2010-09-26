--import NumberTheory.Sieve.Phi
import NumberTheory.Sieve.Factor


totient 1 = 1
totient a = length $ filter (coprime a) [1..a-1]
 where coprime a b = gcd a b == 1

--f n = phi (sieve n) n
{-   
problem248 = (filter (== 6227180929) [totient(n) | n <- [6227180927..]]) !! 1

main = print $ take 1 (filter (== 6227020800) [totient(n) | n <- [6227180927..]])

h [] = 0
h (x:xs) = if x == 6227020800 then x else h xs

g = h [totient(n) | n <- [6227180927..]]
-}

--fst $ unzip $ factor (sieve n) n

--f' :: (Integral a, Fractional a) => a -> a
f' n = let l = fst $ unzip $ factor (sieve n) n
       in floor $ foldr (*) n [1 - (recip p) | p <- l]
