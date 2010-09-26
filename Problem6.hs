
-- Difference between the sum of the squares and the squares of the sum
f :: Int -> Int
f n = let l   = [1..n]
          s1  = (sum l) ^ 2
          s2  = sum (map (^ 2) l)
      in s1 - s2


g = (sum [1..100] ^ 2) - (sum (map (^2) [1..100]))