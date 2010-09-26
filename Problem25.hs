import List


fibs = 0 : 1 : [ x+y | (x,y) <- zip fibs (tail fibs) ]

countFibDigits :: Integer -> Integer
countFibDigits = toInteger . length . show 

f :: Integer -> [Integer] -> Maybe Int
f v (x:xs) = if countFibDigits x == v then elemIndex x fibs else f v xs