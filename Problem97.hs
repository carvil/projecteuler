last10digits = non_mersenne_prime `mod` 10^10
  where non_mersenne_prime = 28433 * 2^(7830457)+1

main = print last10digits 
