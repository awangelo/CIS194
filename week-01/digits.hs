{-# OPTIONS_GHC -Wall #-}

-- sumDigits :: [Integer] -> [Integer]

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
    | n > 0     = (n `mod` 10) : toDigitsRev (n `div` 10)
    | otherwise = []