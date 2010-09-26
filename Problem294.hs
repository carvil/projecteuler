import Char

l n = [i | i <- [1..10^n], div i 23 == 0, f i == 23]


f = sum . map digitToInt . show


g n = let l = [i | i <- [1..(10^n)-1], mod i 23 == 0, f i == 23]
      in sum l

main = print $ g 9
