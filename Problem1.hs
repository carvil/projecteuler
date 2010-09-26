
f = foldr (+) 0 [a | a <- [1..1000], mod a 3 == 0 || mod a 5 == 0]