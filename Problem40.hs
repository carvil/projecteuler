import Char

f = foldr ((*) . digitToInt) 1 l
  where l       = map ((!!) inf_lst) [10^i | i <- [1..6]]
        inf_lst = concat [show i | i <- [0..] ]
