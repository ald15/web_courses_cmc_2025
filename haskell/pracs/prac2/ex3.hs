-- Мы ещё не знаем gcd, поэтому придётся написать свою функцию...(
gcd' :: Int -> Int -> Int
gcd' a 0 = abs a
gcd' a b = gcd' b (a `mod` b)

-- А родной gcd работает с отрицательными числами, поэтому в наш gcd' следует добавить abs
abs' :: Int -> Int
abs' a = if a >= 0 then a else -a

primeTo20 :: [(Int, Int)]
primeTo20 = [(x, y) | x <- [1..20], y <- [2..20], x <= y, gcd' x y == 1]

main :: IO()
main = do
    print("#3: ", primeTo20)