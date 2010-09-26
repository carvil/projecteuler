import Fibonacci
import List

subsequences            :: [a] -> [[a]]
subsequences []         =  [[]]
subsequences (x:xs)     =  subsequences xs ++ map (x:) (subsequences xs)


--zeckendorf n = 
--  let l = takeWhile (<n) fibs
