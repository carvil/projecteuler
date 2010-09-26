
triangleNumber :: Integer -> Integer
triangleNumber n = sum [1..n]

divisors n = [x | x <- [1..n `div` 2], n `mod` x == 0] ++ [n]

triagAbove :: Int -> Integer
triagAbove n = let l = [triangleNumber v | v <- [1..]]
                   k = [x | x <- l, (mod x 2) == 0 || 
                                    (mod x 3) == 0 ||
                                    (mod x 5) == 0]
               in aux n k
                   

aux :: Int -> [Integer] -> Integer
aux n (x:xs) = if factors x >= n then x else aux n xs

-- more than 100 divisors: 73920
main :: IO()
main = print (triagAbove 500)


factors = factorize . zipPrimes . primeFactors

factorize [] = 1
factorize ((n,o):xs) = (o+1) * factorize xs

zipPrimes [] = []
zipPrimes (x:xs) = let (lst,newl) = findEquals x ([],[]) xs
                       len = length(lst)
                   in (x,len) : zipPrimes newl

findEquals v (l,k) [] = (v:l,k)
findEquals v (l,k) (x:xs) = if v == x
                            then findEquals v (x:l,k) xs
                            else findEquals v (l,x:k) xs

primeFactors :: (Integral a) => a -> [a]
primeFactors n = primeFactorsUsingPrimesList (2:[3, 5 .. n `div` 2]) n

primeFactorsUsingPrimesList :: (Integral a) => [a] -> a -> [a]
primeFactorsUsingPrimesList _      1 = []
primeFactorsUsingPrimesList (x:xs) n = 
     if n `rem` x == 0
     then x : primeFactorsUsingPrimesList (x:xs) (n `div` x)
     else primeFactorsUsingPrimesList xs n
primeFactorsUsingPrimesList []     n = [n]