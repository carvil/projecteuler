import NumberTheory.Sieve.Factor
import NumberTheory.Sieve.ONeill
import List

t n 
  | n <= 1    = error "N must be positive!"
  | otherwise = 2 * (n^2) - 1


f m = let s = NumberTheory.Sieve.Factor.sieve (t m)
      in length [t(n) | n <- [2..m], isPrime s (t n) ]

main = print $ f 50000000
