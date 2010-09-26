import NumberTheory.Sieve.Factor


f l x = let l1 = zip l (map (length . factor (sieve x)) l)
            l2 = filter ((==) 4 . snd) l1
            l3 = consec l2
        in l3
  where consec (x1:x2:x3:x4:xs) = 
          case (fst x4) - (fst x1) == 3 of
            True  -> [x1,x2,x3,x4]
            False -> consec (x2:x3:x4:xs)
        consec _ = []

main = print $ f [1..999999] 9999999

