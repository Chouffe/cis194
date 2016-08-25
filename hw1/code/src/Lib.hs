module Lib
    ( validate
    , toDigits
    , toDigitsRev
    , doubleEveryOther
    , sumDigits
    , hanoi
    ) where

-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
   |  x <= 0 = []
   | otherwise = mod x 10 : toDigitsRev (div x 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Exercise 2
doubleEveryOther = reverse . zipWith (\i x ->if odd i then x * 2 else x) [0..] . reverse

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

-- Exercise 4
validate :: Integer -> Bool
validate xs = mod s 10 == 0
  where s = sumDigits $ doubleEveryOther $ toDigits xs

-- Exercise 5: The Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a
