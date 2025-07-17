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

-- const = \x _ -> x


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
(^^^) x 0 = 1
(^^^) x y = x * (x ^^^ (y-1))

euclid :: Int -> Int -> Int
euclid x 0 = x
euclid 0 y = y
euclid x y | x > y     = euclid (x-y) y
           | otherwise = euclid x (y-x)


and' :: [Bool] -> Bool
and' []     = True
and' (x:ys) = x && and' ys

concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

replicatee :: Int -> a -> [a]
replicatee n x
  | n <= 0    = []
  | otherwise = x : replicatee (n - 1) x

(!!!) :: [a] -> Int -> a
(!!!) (x:_) 0  = x
(!!!) (_:xs) n = xs !!! (n-1)
-- if lenth xs > n-1...

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
    | e == x    = True
    | otherwise = elem' e xs

merge' :: Ord a => [a] -> [a] -> [a]
merge' xs []    = xs
merge' [] ys    = ys
merge' xs'@(x:xs) ys'@(y:ys)
    | x > y     = y : merge' xs' ys
    | otherwise = x : merge' xs ys'

msort :: Ord a => [a] -> [a]
msort []       = []
msort [x]      = [x]
msort xs = merge' (msort frst) (msort scnd)
    where frst = take mid xs
          scnd = drop mid xs
          mid  = length xs `div` 2

summ :: Num a => [a] -> a
summ []     = 0
summ (x:xs) = x + summ xs

takee :: Integral a => a -> [b] -> [b]
takee n _
    | n <= 0   = []
takee _ []     = []
takee n (x:xs) = x : takee (n-1) xs

last' :: [a] -> a
last' [x]    = x
last' (_:xs) = last' xs







-- 1 : (2 : (3 : []))
-- para
-- 1 f (2 f (3 f v ))
foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' f v []     = v
foldr'' f v (x:xs) = f x (foldr'' f v xs)


-- 1 : (2 : (3 : []))
-- para
-- 1 * (2 * (3 * 1 ))
vezes :: [Int] -> Int
vezes = foldr (*) 1


-- 1 : (2 : (3 : []))
-- x = ignorar
-- x 1+ (x 1+ (x 1+ 0))
tamanho :: [a] -> Int
-- tamanho = foldr (+ 1) 0
-- `(+1) :: Int -> Int`, mas foldr espera uma funcao :: (a -> Int -> Int).
-- Entao falta ignorar o primeiro argumento `a`:
tamanho = foldr (\_ -> (+ 1)) 0
-- Ou usar      (const (+ 1))


-- 1 : (2 : (3 : []))
-- `λ` faz swap de `x λ y` para `y : x`
-- step by step:
-- 1 λ (2 λ (3 λ []))
-- 2 λ (3 λ [] ++ [1])
-- 3 λ [] ++ [2] ++ [1]
-- [] ++ [3] ++ [2] ++ [1]
reverso :: [a] -> [a]
reverso = foldr (\x xs -> xs ++ [x]) []
-- `\x xs -> xs ++ [x]` eh igual a `snoc x xs = xs ++ [x]`, contrario de cons `:`


-- 1 : (2 : (3 : []))
-- para
-- (([] : 3) : 2) : 1
-- ((v f 3) f 2) f 1
-- ou
-- f (f (f v 1) 2) 3
foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' f v []     = v
foldl'' f v (x:xs) = foldl'' f (f v x) xs
-- Nao funciona para listas infinitas (geralmente),
-- Pois percorre a lista da esquerda para a direita, começando pelo primeiro elemento

-- Sao iguais se a funcao `f` for associativa:
-- foldr (+) 0 [1..5]
-- foldl (+) 0 [1..5]

-- 1 : (2 : (3 : []))
-- para
-- foldr: 1 + (2 + (3 + 0))
--
-- foldl: ((0 + 1) + 2) + 3

-- 1 : (2 : (3 : []))
-- x = ignorar
-- ((0 +1 x) +1 x) +1 x
tamanhol :: [a] -> Int
tamanhol = foldl (\x _ -> x + 1) 0


-- 1 : (2 : (3 : []))
-- para
-- f (f (f v 1) 2) 3
-- : (: (: [] 1) 2) 3
-- : (: (: 1 []) 2) 3
-- : (: 2 (: 1 [])) 3
-- : 3 (: 2 (: 1 []))
-- : 3 (: (2 : (1 : [])))

reversel :: [a] -> [a]
reversel = foldl (\xs x -> x : xs) []
-- Ou apenas    `(flip (:))` para inverter os argumentos do cons


-- False1 : (False2 : (False3 : (False4 : [])))
-- para

-- foldr: False1 && (False2 && (False3 && (False4 && True)))
--        -- começa pelo primeiro elemento (False1) e aninha à direita até o acumulador True
--        -- resultado: False assim que encontra o primeiro False
andr :: [Bool] -> Bool
andr = foldr (&&) True

-- foldl: (((True && False1) && False2) && False3) && False4
--        -- começa pelo acumulador True e vai aplicando da esquerda (False1) para a direita (False4)
--        -- resultado: False assim que encontra o primeiro False
andl :: [Bool] -> Bool
andl = foldl (&&) True


-- REGRA GERAL:
-- * Use foldr para transformar listas em outras listas, especialmente se a lista pode ser infinita.
--   - foldr permite short-circuit (parar cedo), útil para funções como (&&), (||), etc.
--   - foldr funciona bem com listas infinitas.
--
-- * Use foldl' para listas grandes e finitas.
--   - foldl' reverte a ordem implicitamente (foldl' (flip (:)) [] == reverse).
--   - Melhor uso quando a função é associativa/comutativa (+, *, Set.union).
--   - Não pode short-circuitar, sempre percorre toda a lista.
--
-- * foldl é raramente recomendado; geralmente prefira foldl'.
--
--
-- Resumo:
-- - foldr: listas infinitas, short-circuit, mantém ordem.
-- - foldl': listas grandes e finitas, performance, computa pela ordem reversa.
