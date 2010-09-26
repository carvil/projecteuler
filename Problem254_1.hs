import Char

-- Efficient. Integer gives good range values
fac :: Int -> Integer
fac n = foldr (*) 1 [1..(toInteger n)]

-- sum of the factorial of each digit of a number
f :: Int -> Integer
f = sum . map (fac . digitToInt) . show 

-- sum of each digit of a number
sf :: Int -> Int
sf = foldr (+) 0 . map digitToInt . show . f

l :: [Integer]
l = [2000000..]

--Define g(i) to be the smallest positive integer n such that sf(n) = i
g :: Int -> Int
g i = g' i l

g' :: Int -> [Integer] -> Int
g' i (x:xs) = if sf(fromInteger x) == i then fromInteger x else g' i xs

test :: IO()
test = t 41 l

-- similar to g, but prints something (when I was not sure about number bounds)
t :: Int -> [Integer] -> IO()
t n (x:xs) = let v = sf(fromInteger x)
             in if v == n 
                then print (fromInteger x) 
                else do print v
                        t n xs

-- g 20 ==> exists n: sf(n) = 20
-- g is inverse function of sf

sg = foldr (+) 0 . map digitToInt . show . g


-- What is sum sg(i) for 1 <= i <= 150?

-- from 1 to 40 = 468
result = let l = [1..40]
             k = map sg l
         in sum k

result2 = let j = [41..50]
              k = map sg j
          in sum k

main :: IO()
main = print result