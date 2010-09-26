import List


proper_divisors n = [ x | x <- [1..n], mod n x == 0, n /= x]


amicable = sum [ x | x <- [1..10000], f(x)]
  where f x = let y = sum $ proper_divisors x
                  z = sum $ proper_divisors y
              in x == z && x /= y


main = print amicable
