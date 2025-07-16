module HuttonBook where

sum' :: Num a => [a] -> a
sum' [x]    = x
sum' (x:xs) = x + sum' xs

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                   smaller = [a | a <- xs, a <= x]
                   larger  = [b | b <- xs, b > x]

n = a `div` length xs
        where
            a = 10
            xs = [1,2,3,4,5]



-- 1 :: [Char]

-- 2 :: (Char, Char, Char)

-- 3 :: [(Bool, Char)]

-- 4 :: ([Bool], [Char])

-- 5 :: [[a] -> [a]]


bools = [True]

nums = [[1]]

add a b c = a + b + c

copy a = (a,a)

apply f a = f a

twice f x = f (f x)


second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

const = \x _ -> x


odds :: Int -> [Int]
odds n = map (\x -> x*2 + 1) [0..n-1]







halve' :: [a] -> ([a],[a])
halve' xs = (take n xs, drop n xs)
            where n = length xs `div` 2

third :: [a] -> a
third = head . tail . tail

third' :: [a] -> a
third' = (!! 2)

third'' :: [a] -> a
third'' (_:_:x:_) = x


safetail :: [a] -> [a]
safetail xs = if null xs
              then []
              else tail xs

safetail' :: [a] -> [a]
safetail' xs
    | null xs   = []
    | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs


(||) :: Bool -> Bool -> Bool
False || False = False
_     || _     = True


(&&&) :: Bool -> Bool -> Bool
a &&& b = if a
         then b
         else a


mult :: Int -> Int -> Int -> Int
mult = \x y z -> x*y*z


luhnDouble :: Int -> Int
luhnDouble n
    | dbl > 9   = dbl - 9
    | otherwise = dbl
    where dbl = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn i j k l = (i + j + k + l) `mod` 10 == 0







lowerCase :: String -> Int
lowerCase str = length [c | c <- str, c >= 'a' && c <= 'z' ]

sumSquared :: Int
sumSquared = sum [x*x | x <- [1..100]]

grid :: Int -> Int -> [(Int,Int)]
grid n m = [(x, y) | x <- [0..n], y <- [0..m]]

square :: Int -> [(Int,Int)]
square n = [(x, y) | (x,y) <- grid n n, x /= y]

replicate' :: Int -> a -> [a]
replicate' n e = [e | _ <- [1..n]]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x, y, z) | x <- ns, y <- ns, z <- ns, isPyth x y z]
            where ns = [3..n]

isPyth :: Int -> Int -> Int -> Bool
isPyth x y z = x^2 + y^2 == z^2

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x]

factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x,y) <- zip xs ys]

getIndex :: Eq a => a -> [a] -> Int
getIndex e xs = head [i | (y,i) <- zip xs [0..], e == y]








drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' 0 xs     = xs
drop' n (_:xs) = drop (n-1) xs



fac :: Int -> Int
fac 0 = 1
fac n
    | n < 0     = 1
    | otherwise = n * fac (n-1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

(^^^) :: (Num a, Integral b) => a -> b -> a
(^^^) x 1 = x
(^^^) x y = x * x ^^^ (y-1)

euclid :: Int -> Int -> Int
euclid x 0 = x
euclid 0 y = y
euclid x y | x > y     = euclid (x-y) y
           | otherwise = euclid x (y-x)


and' :: [Bool] -> Bool
and' [True]  = True
and' [False] = False
and' (x:ys)  = x && and' ys

concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

replicatee :: Int -> a -> [a]
replicatee 1 x = [x]
replicatee n x = x : replicatee (n-1) x

(!!!) :: [a] -> Int -> a
(!!!) (x:_) 0  = x
(!!!) (x:xs) n = (!!!) xs (n-1)
-- if lenth xs > n-1...

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
    | e == x    = True
    | otherwise = elem' e xs
