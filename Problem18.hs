import List
import Examples

type Triag = [[Int]]

triangle :: Triag
triangle = 
 [
  [75],
  [95,64],
  [17,47,82],
  [18,35,87,10],
  [20,04,82,47,65],
  [19,01,23,75,03,34],
  [88,02,77,73,07,63,67],
  [99,65,04,28,06,16,70,92],
  [41,41,26,56,83,40,80,70,33],
  [41,48,72,33,47,32,37,16,94,29],
  [53,71,44,65,25,43,91,52,97,51,14],
  [70,11,33,28,77,73,17,78,39,68,17,57],
  [91,71,52,38,17,14,91,43,58,50,27,29,48],
  [63,66,04,68,89,53,67,30,73,16,69,87,40,31],
  [04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]
 ]

triangleExample :: Triag
triangleExample = 
  [
   [3],
   [7,4],
   [2,4,6],
   [8,5,9,3]
  ]

maxValue ::
  Int   -> --position
  [Int] -> --list
  Int

maxValue p l =
  let a = (!!) l p
      b = (!!) l (p+1)
  in case a >= b of 
      True  -> p
      False -> p+1

indexOf ::
  Int   -> --element
  [Int] -> --list
  Int      --position in the list
indexOf n l = 
  case (elemIndex n l) of
   Nothing -> 0
   Just x -> x

lookahead :: 
  [Int] ->  -- Line 1
  [Int] ->  -- Line 2
  Int   ->  -- Column value
  Int       -- Column position where to go next

lookahead l1 l2 c = 
  let x1_l1 = (!!) l1 c  
      x2_l1 = (!!) l1 (c+1) 
      v1    = (x1_l1 + (!!) l2 c,c)      
      v2    = (x1_l1 + (!!) l2 (c+1),c)
      v3    = (x2_l1 + (!!) l2 (c+1),c+1)
      v4    = (x2_l1 + (!!) l2 (c+2),c+1) 
      l     = [v1,v2,v3,v4]
  in maxOf l


maxOf l = let k = unzip l
              m = (maximum . fst) k
              n = indexOf m (fst k)
          in (!!) (snd k) n


total_max :: 
  Triag -> -- Triangle
  Int   -> -- Next column
  Int      -- maximum total

total_max (l1:l2:l3:ls) n = 
  let next = lookahead l2 l3 n
      val  = (!!) l1 n
  in val + total_max (l2:l3:ls) next
total_max (l1:l2:[]) n = 
  let val = (!!) l1 n
      nextval = maxValue n l2
  in val + (!!) l2 nextval


total = total_max triangle 0

