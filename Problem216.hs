import System.Time
import Control.Parallel

t :: Integer -> Integer
t n = 2 * (n^2) - 1

{-
f m = let s = NumberTheory.Sieve.Factor.sieve (t m)
      in length [t(n) | n <- [2..m], isPrime s (t n) ]
-}

f :: [Integer] -> Int
f = length . filter isPrime . map t

isPrime :: Integer -> Bool
isPrime = isPrime' 3

isPrime' :: Integer -> Integer -> Bool
isPrime' i n  
  | n < 2 = False
  | n `mod` 2 == 0 = n == 2
  | i*i <= n = if n `mod` i == 0 then False else isPrime' (i+2) n
  | otherwise = True

main = do t0 <- getClockTime
          pseq r1 (return ())
          t1 <- getClockTime
          putStrLn ("Total: " ++ show r1)
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds.")

  where secDiff :: ClockTime -> ClockTime -> Float
        secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = 
           fromInteger (psecs2-psecs1) / 1e12 + fromInteger (secs2-secs1)

        r1 = par_r1

par_r1 = r1 `par` (r2 `pseq` (r1 + r2))
  where r1 = f [2..25000]
        r2 = f [25001..50000]

--print $ f [2..50000000]
