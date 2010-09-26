import List
import Char

nreverse :: Integer -> Integer -> Integer
nreverse n acc | n < 10 = n + (10 * acc)
nreverse n acc = nreverse (div n 10) ((mod n 10) + (10 * acc))


--nreverse :: Int -> Int
--nreverse = readInt . reverse . show

--readInt :: String -> Int
--readInt = read

reversible [] = 0
reversible (x:xs) = 
  let n = nreverse x 0
  --    m = n+x
  --    b = map odd $ map digitToInt $ show m
  in case oddDigs (n+x) of
      True  -> 1 + reversible xs
      False -> reversible xs

oddDigs :: Integer -> Bool
oddDigs n | n < 10 = mod n 2 == 1
oddDigs n = (mod (mod n 10) 2) == 1 && oddDigs (div n 10)


odds n | n < 10 = mod n 2 == 1
odds n = let x = mod n 10
             y = mod x 2
         in if x == 1 then odds (div n 10) else False

main = print $ 120 + 2*(reversible [1001,1003..99999999])
