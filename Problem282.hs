module Main where


mod_ackermann i j 
  | i == 0          = (j+1) `mod` (14^8)
  | j == 0 && i > 0 = mod_ackermann (i-1) j
  | i > 0 && j > 0  = mod_ackermann (i-1) (mod_ackermann i (j-1))


ackermann m n | m == 0 = n+1
              | m > 0 && n == 0 = ackermann (m-1) 1
              | m > 0 && n > 0 = ackermann (m-1) (ackermann m (n-1))



a m n | m == 0 = n+1
      | m > 0 && n == 0 = a (m-1) 1
      | m > 0 && n > 0 = a (m-1) (a m (n-1))


main = print $ a 6 6 

{-
import Array
import Data.Map
import Control.Monad
import Control.Monad.State
import List

type StateMap a b = State (Map a b) b 

-- ackermann function
a m n | m == 0 = n+1
      | m > 0 && n == 0 = a (m-1) n
      | m > 0 && n > 0 = a (m-1) (a m (n-1))


--ackermann wrote in a fix point style
ackermann' :: ((Integer,Integer) -> Integer) -> 
              ((Integer,Integer) -> Integer)
ackermann' f (m,n) | m == 0 = n+1
                   | m > 0 && n == 0 = f (m-1,1)
                   | m > 0 && n > 0 = f (m-1, f (m,n-1))


-- intermidiate structure to store calculated values (call-by-need)
ackermann m n = let a = memoize ((0,0),(6,6)) ackermann'
                in a Array.! (m,n)

memoize :: Ix a => (a,a) -> ((a -> b) -> (a -> b)) -> Array a b
memoize vals ack = ar
  where ar = listArray vals [ack g x | x <- range vals]
        g n = case (inRange vals n) of
                True -> ar Array.! n
                False -> ack g n  


-- MONADIC VERSION WITH MAP
ackermann'' f (m,n) | m == 0 = return (n+1)
                    | m > 0 && n == 0 = f (m-1,1)
                    | m > 0 && n > 0 = f (m,n-1) >>= \res -> f (m-1, res)

memoize' :: Ord a => ((a -> StateMap a b) -> (a -> StateMap a b)) -> (a -> b) 
memoize' ack vals = evalState (a vals) empty
  where ack' n = do nval <- ack a n
                    mp'  <- get
                    put $ Data.Map.insert n nval mp';
                    return nval
        a n = do mp <- get
                 maybe (ack' n) return (Data.Map.lookup n mp)


run_ackermann m n = memoize' ackermann'' (m,n)

ackerM :: Monad m => ((Integer, Integer) -> m Integer) -> 
                     ((Integer, Integer) -> m Integer) 
ackerM a' (0,n) = return $ n + 1 
ackerM a' (m,0) = a' (m-1, 1) 
ackerM a' (m,n) = a' (m, n-1) >>= \n' -> a' (m-1, n') 


{-mp = fromList [((0,0),1),((0,1),2),((0,2),3),((0,3),4),((0,4),5),
               ((1,0),2),((1,1),3),((1,2),4),((1,3),5),((1,4),5)]
-}

memoizeM :: Ord a => ((a -> StateMap a b) -> (a -> StateMap a b)) -> (a -> b) 
memoizeM t x = evalState (f x) empty where  
  g x = do 
    y <- t f x   
    m <- get 
    put $ Data.Map.insert x y m; 
    return y 
  f x = get >>= \m -> maybe (g x) return (Data.Map.lookup x m) 


ackermann2 :: Integer -> Integer -> Integer 
ackermann2 m n 
  | n < 0 || m < 0  = error "negative arguments" 
  | otherwise       = memoizeM ackerM (m,n)


main = print $ (sum [ackermann2 n n | n <- [0..6]]) `mod` (14^8)

-}
