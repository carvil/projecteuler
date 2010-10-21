import List (maximum)

f :: (Integer,Integer) -> Integer
f (a,n) = let x = (a-1)^n + (a+1)^n
              y = a^2
          in rem x y

g a = [1..2000] >>= (\y -> return (a,y))


h a | odd a = (a-1)*a
    | otherwise = (a-2)*a

main = print $ sum $ map h [3..1000]

--print $ sum [ maximum (map f (g a)) | a <- [3..1000]]


