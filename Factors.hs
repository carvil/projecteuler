module  Factors
     (  FactorSieve()
     ,  findFactor
     ,  sieveBound
     ,  isPrime
     ,  factor
     ,  sieve
     )  where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.MArray
import Data.Array.Unboxed

import Data.Bits
import Data.Word

-- Note that if you want to sieve numbers beyond 2^32,  you probably do not
-- want to simply change this type to (UArray Word64 Word32),  as this would
-- result in 4 GB of wasted space.   Increasing the limit in a sensible
-- fashion would require the use of multiple arrays with heterogenous types.

newtype FactorSieve = FactorSieve (UArray Word32 Word16)

instance Show FactorSieve where
   show fs = "<<FactorSieve " ++ show (sieveBound fs) ++ ">>"

instance Eq FactorSieve where
    a == b  =  sieveBound' a == sieveBound' b

instance Ord FactorSieve where
   compare a b = compare (sieveBound' a) (sieveBound' b)

-- |  Returns the upper limit of a sieve.
sieveBound :: Integral a => FactorSieve -> a
sieveBound (FactorSieve arr) = fromIntegral (shiftL (snd (bounds arr)) 1)

sieveBound' (FactorSieve arr) = (snd (bounds arr))

-- |  Is a number prime?
isPrime :: Integral a => FactorSieve -> a -> Bool
isPrime (FactorSieve arr) n
   | even n    = n == 2
   | n <= 1    = False
   | otherwise = arr ! ix (fromIntegral n) == 0

-- |  Returns the smallest prime divisor of a given number in the sieve.
findFactor :: Integral a => FactorSieve -> a -> a
findFactor (FactorSieve arr) n
    | even n = 2
    | d == 0 = n
    | otherwise = fromIntegral d
      where
        d = arr ! ix (fromIntegral n)

ix n = shiftR n 1

{-# INLINE for #-}
for min max step f = loop min
    where
      loop !x
          | x <= max  = f x >> loop (x + step)
          | otherwise = return ()

flsqrt x = floor (sqrt (fromIntegral x))

-- |  Finds the smallest prime divisor of every number up to a given limit.
sieve :: (Integral a, RealFrac a) => a -> FactorSieve
sieve limit
    | not (0 <= intlim && intlim < 2^32)
      = error ("NumberTheory.Sieve.Factor.sieve: out of bounds: "  ++ show limit)
    | otherwise
      = FactorSieve $ runSTUArray $ do
          arr <- newArray (ix 3, lim) 0
          for 3 (flsqrt limit) 2
              (\i -> do
                 i' <- readArray arr (ix i)
                 when (i' == 0)
                      (for (ix (i*i)) lim i
                           (\i' -> do
                              i'' <- readArray arr i'
                              when (i'' == 0)
                                   (writeArray arr i' (fromIntegral i)))))
          return arr
    where
      intlim  = fromIntegral limit :: Integer
      lim = ix (fromIntegral limit - 1) :: Word32

-- FIXME:  clean up the definition of 'factor', but maintain speed

-- |  Factors a number completely using a sieve, e.g.
--
-- > factor (sieve 1000) 360 == [(2,3),(3,2),(5,1)]
factor :: (Integral a, RealFrac a) => FactorSieve -> a -> [(a,a)]
factor (FactorSieve arr) n = start (fromIntegral n)
   where
     p x y = ((,) $! fromIntegral x) $! fromIntegral y

     start n
         | even n    = loop0 (shiftR n 1) 1
         | otherwise = loop1 n

     loop0 !n !i
         | even n    = loop0 (shiftR n 1) (i + 1)
         | otherwise = p 2 i : loop1 n

     loop1 !n
         | n == 1    = []
         | d == 0    = [p n 1]
         | otherwise = loop1' (n `div` d) d 1
           where
             d = fromIntegral (arr ! ix n)

     loop1' !n !d !i
         | d' == 0   = if n == d then [p d (i+1)] else [p d i, p n 1]
         | d' == d   =         loop1' (n `div` d ) d  (i+1)
         | otherwise = p d i : loop1' (n `div` d') d'    1
           where
             d' = fromIntegral (arr ! ix n)

