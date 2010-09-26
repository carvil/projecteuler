import List

main = do x <- readFile "names.txt"
          y <- readNames x
          print ((foldr (+) 0 . values 1 . sort) y)

readNames :: String -> IO [String]
readNames = readIO

getPosition x = case (elemIndex x ['A','B'..'Z']) of
                 Nothing -> 0
                 Just n -> n+1

values :: Int -> [String] -> [Int] values _ [] = [] values n (x:xs) =
(n * (value x)) : values (n+1) xs

value = foldr (+) 0 . map getPosition