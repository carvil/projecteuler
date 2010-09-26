

-- What is the smallest number that is evenly divisible 
-- by all of the numbers from 1 to 20?

f [] = 0
f (x:xs) = if condition x then x else f xs

h = f [230000000..250000000]

-- Returns a boolean expressing the property of a number being divisible by
-- the first 20 numbers with reminder = 0

condition :: Int -> Bool
condition = foldr (&&) True . apply
  where apply n = [ mod n x == 0 | x <- [1..20]]


-- efficient one:

g = foldr lcm 1 [1..20]