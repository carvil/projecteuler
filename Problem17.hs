import Char

sd 1 = "one"
sd 2 = "two"
sd 3 = "three"
sd 4 = "four"
sd 5 = "five"
sd 6 = "six"
sd 7 = "seven"
sd 8 = "eight"
sd 9 = "nine"
sd 10 = "ten"
sd 11 = "eleven"
sd 12 = "twelve"
sd 13 = "thirteen"
sd 14 = "fourteen"
sd 15 = "fifteen"
sd 16 = "sixteen"
sd 17 = "seventeen"
sd 18 = "eighteen"
sd 19 = "nineteen"
sd 20 = "twenty"
sd 30 = "thirty"
sd 40 = "forty"
sd 50 = "fifty"
sd 60 = "sixty"
sd 70 = "seventy"
sd 80 = "eighty"
sd 90 = "ninety"
sd n = let l = show n
           f = head l
           r = tail l
           v = length l
           m = if toInt r == 0 then "" else "and"
       in case v of
           2 -> sd (toInt (f : "0")) ++ sd (digitToInt (head r))
           3 -> sd (toInt (f:"")) ++ "hundred" ++ m ++ sd (toInt r)
           4 -> sd (toInt (f:"")) ++ "thousand" ++ sd(toInt r)
           _ -> ""


toInt :: String -> Int
toInt x = read x


numOfLetters = length . concat . map sd