import List
import Char

result = maximum [g (a^b) | a <- [1..99], b <- [1..99]] 
  where g = sum . map digitToInt . show

main = print $ result
