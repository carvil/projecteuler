import Char

factorial :: Int -> Int
factorial n = foldr (*) 1 [1..n]

f :: Int -> Int
f = sum . map (factorial . digitToInt) . show 

sf :: Int -> Int
sf = foldr (+) 0 . map digitToInt . show . f

--Define g(i) to be the smallest positive integer n such that sf(n) = i
g :: Int -> Int
g i = let l = [1..]
      in g' i l
  where g' i (x:xs) = if sf(x) == i then x else g' i xs

sg = foldr (+) 0 . map digitToInt . show . g


-- What is sum sg(i) for 1 <= i <= 150?

result = let l = [1..50]
             k = map sg l
         in sum k

main :: IO()
main = print result