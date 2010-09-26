import List

-- List of palindromes resulted in the multiplication of 2 numbers with 3 digits


palindrome = [ n*m | n <- [100..999], m <- [100..999], show (n*m) == reverse (show (n*m)) ]

-- Largest palindrome

lpal = (last . sort) palindrome