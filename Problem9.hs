l = [ [a,b,c] | a <- [1..999], b <- [1..999], c <- [1..999], a+b+c== 1000 && a^2 + b^2 == c^2 && a < b  && b < c]

h = [[a,b,1000-a-b] | a <- [1..999], b <- [1..999],a^2 + b^2 == (1000-a-b)^2]

value = product . head


-- value l
-- (371.90 secs, 32855979636 bytes)

-- value h
-- (0.79 secs, 108452856 bytes)

