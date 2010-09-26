


isPerfectSquare n = let x = sqrt n
                    in x^2 == n


f = take 1 [(x,y,z) | x <- [1..],y <-[1..],z <- [1..],
             isPerfectSquare (x+y),
             isPerfectSquare (x-y),
             isPerfectSquare (x+z),
             isPerfectSquare (x-z),
             isPerfectSquare (y+z),
             isPerfectSquare (y-z)]

main = print $ f
