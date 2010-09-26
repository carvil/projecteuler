import Char


f = sum [x | x <- [2..10^6], g x == x]
  where g = sum . map (^5) . map digitToInt . show

main = print f
