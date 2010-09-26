primes2m = sieve ([2] ++ [3,5..2000000])

sieve [] = 0
sieve (p:xs) = p + sieve [x | x<-xs, x `mod` p /= 0]

l = [2] ++ [3,5..20000]

primes [] = 0
primes (x:xs) = case isPrime x of
                  True -> x + primes xs
                  _    -> primes xs

isPrime n = length (divisors n) == 2

divisors n = [ x | x <- [1..n], mod n x == 0]

main :: IO ()
main = print primes2m
