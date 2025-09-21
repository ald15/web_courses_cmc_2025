-- Разбор prac1


-- reverse :: [a] -> [a]
-- reverse l = rev l []
-- rev :: [a] -> [a] -> [a]
-- rev [] l2 = l2
-- rev (x:xs) l2 = rev xs (x:l2)

-- rev1 :: [a] -> [a] -> [a]
-- rev1 (x:xs) = rev(xs ++ [x])
-- rev1 [] = []

-- zipWith'(x:xs) (y:ys) = f x y: zipWith' f xs ys
-- zipWith' _ _  _ = []

-- Безымянная функция + выводятся остальные аргументы по 1 аргументу c = (a, b)
-- и существующей сигнатуре
zip' = zipWith (\x y -> (x, y))

-- pl2 x = x + 2
-- pl2 = \x -> x + 2
-- pl2 = (+2)
-- pl2 = (2+)