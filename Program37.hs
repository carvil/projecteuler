import NumberTheory.Sieve.ONeill

f = let l = primes
        k = filter g l
    in take 11 k

g n = let l = truncatable n
      in foldr (&&) True l


truncatable n = let l = trunc (show n)
                    r = trunc (reverse $ show n)
                    k = l ++ r
                in map (`elem` primes) k
