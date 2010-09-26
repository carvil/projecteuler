f p q = let phi = (p-1)*(q-1)
        in sum [e | e <- [2..phi-1],
                gcd e phi == 1,
                (gcd (e-1) (p-1)) +1 == 3,
                (gcd (e-1) (q-1)) +1 == 3
               ]


main = print $ f 1009 3643
