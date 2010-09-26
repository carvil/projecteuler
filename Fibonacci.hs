module Fibonacci (fibs, goldenFibs) where

--Infinite list of fibonacci numbers
fibs = 0 : 1 : [ x+y | (x,y) <- zip fibs (tail fibs) ]

--Nth fibonacci number using the golden ratio
goldenFibs n = 
  let g = (1 + sqrt(5))/2
  in round $ (g^n - (1-g)^n) / sqrt(5)
