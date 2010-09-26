import List
import Char

f :: Integer -> Integer
f n = read $ head $ dropWhile ((/= True) . g) [show (n*i) | i <- [1..]]

g x =all h x
h y = y=='0' || y=='1' || y=='2'

main = print $ sum $ [div (f n) n | n <- [1..100]]
