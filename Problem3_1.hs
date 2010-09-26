-- What is the largest prime factor of 600851475143?

-- Returns a list of the divisors of a number n
divisors n = [ x | x <- [1..n], mod n x == 0]

-- Verifies if a number is prime
isPrime n = case length (divisors n) of
              2 -> True
              _ -> False

-- List of first n primes
l n = 2: take (n-1) [v | v <- [3,5..], isPrime v]

----------------------------------------------------
-- Regular sieve algorithm


prime10001 = (2 : (sieve [3,5..])) !! 10001

sieve [] = []
sieve (x:xs) = x : sieve [i | i <- xs, mod i x /= 0]
-- (68.71 secs, 4023321224 bytes)

primesB2M = sum (take 2000000 (2 : (sieve [3,5..])))

-- Sieve of Atkin

limit = 10001
sqrtLimit = sqrt limit
is_prime i = elem i [5..limit]


--f1 x y = if x <= sqrtLimit then 

{-

// arbitrary search limit
limit ← 1000000         

// initialize the sieve
is_prime(i) ← false, i ∈ [5, limit] 

// put in candidate primes: 
// integers which have an odd number of
// representations by certain quadratic forms
for (x, y) in [1, √limit] × [1, √limit]:
    n ← 4x²+y²
    if (n ≤ limit) and (n mod 12 = 1 or n mod 12 = 5):
        is_prime(n) ← ¬is_prime(n)
    n ← 3x²+y²
    if (n ≤ limit) and (n mod 12 = 7):
        is_prime(n) ← ¬is_prime(n)
    n ← 3x²-y²
    if (x > y) and (n ≤ limit) and (n mod 12 = 11):
        is_prime(n) ← ¬is_prime(n)
  
// eliminate composites by sieving
for n in [5, √limit]:
    if is_prime(n):
        // n is prime, omit multiples of its square; this is
        // sufficient because composites which managed to get
        // on the list cannot be square-free
        is_prime(k) ← false, k ∈ {n², 2n², 3n², ..., limit} 

print 2, 3
for n in [5, limit]:
    if is_prime(n): print n

-}



---------------------------------------------------

primes = sieve' [2..]

primes2m = sieve' [2..2000000]

sieve' (p:xs) = p : sieve' [x | x<-xs, x `mod` p /= 0]

main :: IO ()
main = print (sum primes2m)

-- Prime factors of first n primes
factors n = foldr (*) 1 (l n)

-- Solve the problem now :)
--factors n = 600.851.475.143

g max = [ n | n <- l max,factors n == 600851475143 ]

biggestDivisor n
              | even n    = biggestDivisor (div n 2)
              | otherwise = m n 3
                where m n d
                          | d >= n        = d
                          | mod n d == 0  = m (div n d) d
                          | otherwise     = m n (d + 2)