-- Без main, т.к. запускалось в интерпретаторе

last' :: [a] -> a
last' [l] = l
last' (_:ls) = last ls


init' :: [a] -> [a]
init' [x] = []
init' (x:xs) = x:init xs


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x:map' f xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if f x 
                    then x:filter' f xs
                    else filter' f xs

-- Решение через охранное выражение
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f (x:xs) | f x = x:filter'' f xs
                    | otherwise = filter'' f xs


sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) =  x + sum' xs


minimum' :: (Ord a) => [a] -> a
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)


-- foldl f ac [l1, l2, ... , ln] -> (f (f ac e1) e2) ... en

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' _ ac [] = ac
foldl'' f ac (x:xs) = foldl'' f (f ac x) xs


foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' _ ac [] = ac
foldr'' f ac (x:xs) = f x (foldr'' f ac xs)

-- [TODO]
-- foldl1'' :: (a -> a -> a) -> [a] -> a
-- foldl1'' f (x:xx:xxs) = foldl1'' (f x xx) 

-- foldr1 :: (a -> a -> a) -> [a] -> a


-- \[ (f \cdot g) x\ = f (g x)] -- композиция функций
-- [TODO]
-- .' :: (b -> c) -> (a -> b) -> (a -> c)
-- .' f g 



ff :: (Num a, Ord a ) => [a] ->  [a]
ff l = map (+10) (filter (>5) l)

-- Аналогиное решение, но через композицию функций
fff :: (Num a, Ord a ) => [a] ->  [a]
fff = map (+10) . filter (>5)


-- Генерируемые списки
-- [1..10] -> [1,2,3,4,5,6,7,8,9,10]
-- [1, 3..10]  -> [1,3,5,7,9]
-- [1..] - бесконечный список натуральных чисел
-- [elem | start, condition] - генератор списка


ones :: [Int]
ones = [1 | i <- [1..]]
-- ИЛИ ones = 1:ones

repeat' :: a -> [a]
repeat' x = [x | _ <- [1..]]
-- ИЛИ x = x:repeat x

cycle' :: [a] -> [a]
cycle' l= l ++ cycle l


-- map и filter  через генератор
filter''' f l = [x | x <- l, f x]
map''' f l = [f x | x <- l]

-- Пифагоровы тройки, в данном случаи их 52
p3 = [(x, y, z) | x <- [1..100], y <- [1..100], z <- [1..100], x^2+y^2==z^2, x <= y]


dec' l1 l2 = [(x, y) | x <- l1, y <- l2]


fib' :: [Int]
fib' = fib1 1 1
fib1 a b = a:fib1 b (a+b)


rep' :: (a -> a) -> a -> [a]
rep' f x = x:rep' f (f x)