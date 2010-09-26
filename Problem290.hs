module Main where

import Char
import Control.Parallel
import Control.Parallel.Strategies



f = sum . map digitToInt . show

problem290' = length (parBuffer 20 rwhnf [x | x <- [0..(10^10)-1], f x == f (137*x)])

main = print $ g

problem290'' = let l = parBuffer 50 rwhnf [x | x <- [0..(10^10)], f x == f (137*x)]
               in length l


-- 0..(10^7)-1 - 271523


g = g' $ parBuffer 1000 rwhnf [(10^7)..(10^12)-1]

g' [] = 0
g' (x:xs) |  f x == f (137*x) = 1 + g' xs
          | otherwise = g' xs
