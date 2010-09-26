
-- Infinite list of fibonacci numbers
fibs = 0 : 1 : [ x+y | (x,y) <- zip fibs (tail fibs) ]

-- List containing the fibonacci numbers smaller than 4.000.000 
l = take 34 fibs

-- Function returning the sum of the even numbers of the fibonacci
-- sequence that are smaller than 4.000.000
f = foldr (+) 0 (filter even l)



--Using the golden ration
goldenRatio = 1.6180339887


v = goldenRatio ^ 3

h r = case r > 4000000 of
       False -> r : h (round ((fromInteger r)*v))
       _     -> []
      
g = foldr (+) 0 (h 2)


