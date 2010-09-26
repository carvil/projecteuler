import Numeric
import Char
import List


f i = isPalindrome (show i) && isPalindrome (showIntAtBase 2 intToDigit i "")
  where isPalindrome l = l == reverse l


main = print $ sum [i | i <- [1..10^6], f i]
