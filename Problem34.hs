import Char

f = sum [x | x <- [3..50000], x == g x]
  where g = sum . map (factorial . digitToInt) . show
        factorial n = foldl (*) 1 [1..n]
  
main = print f
