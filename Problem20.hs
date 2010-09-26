import Char

-- n! means n *  (n - 1) * ... * 3 * 2 * 1

-- Factorial function
fac n = foldr (*) 1 [1..n]

-- sum of the digits of the factorial of 100
sumF = sum . map digitToInt . show . fac

