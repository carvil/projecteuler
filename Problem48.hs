

myZip l = aux 1 l
  where aux _ [] = []
        aux n (x:xs) = (x,n) : aux (n+1) xs

series :: [Integer] -> Integer
series = foldr (+) 0 . map sq
  where sq x = x^x


lastTen l = let lst = show (series l)
                len = length lst
            in drop (len-10) lst
            
