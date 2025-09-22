-- #1
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

minimum' :: (Ord a) => [a] -> a
minimum' [l] = l
minimum' l = foldl1 (min) l

maximum' :: (Ord a) => [a] -> a
maximum' [l] = l
maximum' l = foldl1 (max) l

map' :: (a -> b) -> [a] -> [b]
map' f l = foldr (\x ac -> f x : ac) [] l

filter' :: (a -> Bool) -> [a] -> [a]
filter' f l = foldr (\x ac -> if f x then x:ac else ac) [] l



-- #2
-- Генератор списков
pow2 :: [Int]
pow2 = [2 ^ x | x <- [0..]]

-- Функция-генератор
pow2' :: Num t => [t]
pow2' = pow2_ 1
pow2_ :: Num t => t -> [t]
pow2_ ac = ac:pow2_ (2 * ac)



-- #3
-- Мы ещё не знаем gcd, поэтому придётся написать свою функцию...(
gcd' :: Int -> Int -> Int
gcd' a 0 = abs a
gcd' a b = gcd' b (a `mod` b)

-- А родной gcd работает с отрицательными числами, поэтому в наш gcd' следует добавить abs
abs' :: Int -> Int
abs' a = if a >= 0 then a else -a

primeTo20 :: [(Int, Int)]
primeTo20 = [(x, y) | x <- [1..20], y <- [2..20], x <= y, gcd' x y == 1]



-- #4
path :: [(Float, Float)] -> Float
path l = sum (zipWith d2dots l (tail l))

d2dots :: (Float, Float) -> (Float, Float) -> Float
d2dots (a, b) (c, d) = sqrt ((a-c)^2 + (b-d)^2)



-- #5
expC :: Double -> [Double]
expC x = [ (x ^ i) / ((1:fact 2 1) !! i) | i <- [0..]]

fact :: Num t => t -> t -> [t]
fact s n =  n:fact (s + 1) (n * s)

exp' :: Double -> Int -> Double
exp' x n = sum(take (n + 1) (expC x))



-- Tests
main :: IO()
main = do
    print("#1-1: ", sum' [1, 2, 3, 4, 5])
    print("#1-2: ", minimum' [1, 2, 3, 4, -10, 5])
    print("#1-3: ", maximum' [1, 2, 3, 4, -10, 5])
    print("#1-4: ", map' (*2) [1, 2, 3, 4, 5])
    print("#1-5: ", filter' (>3) [1, 2, 3, 4, 5])
    print("#2-1: ", take 10 pow2)
    print("#2-2: ", take 10 pow2')
    print("#3: ", primeTo20)
    -- Случайный набор
    print("#4-1: ", path [(1, 2), (3, 4), (5, 6), (7, 8), (9, 10)])
    -- Периметр прямоугольника со сторонами 3 и 4
    print("#4-2: ", path [(0, 0), (0, 3), (4, 3), (4, 0), (0, 0)])
    print("#5-1: ", take 10 (expC 10))
    print("#5-2: ", exp' 10 50)