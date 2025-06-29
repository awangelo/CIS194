{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-} -- Ignorar dica de foldr em `sumDigits`

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

sumDigits :: [Integer] -> Integer
sumDigits []       = 0
sumDigits (x : xs) = sum (toDigits x) + sumDigits xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleOther (reverse xs))

doubleOther :: [Integer] -> [Integer]
doubleOther (x : y : zs) = x : (y * 2) : doubleOther zs
doubleOther [x]          = [x]
doubleOther _            = []

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n > 0 = (n `mod` 10) : toDigitsRev (n `div` 10)
    | otherwise = []
