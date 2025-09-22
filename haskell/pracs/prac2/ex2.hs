-- Генератор списков
pow2 :: [Int]
pow2 = [2 ^ x | x <- [0..]]

-- Функция-генератор
pow2' :: Num t => [t]
pow2' = pow2_ 1
pow2_ :: Num t => t -> [t]
pow2_ ac = ac:pow2_ (2 * ac)


main :: IO()
main = do
    print("#2-1: ", take 10 pow2)
    print("#2-2: ", take 10 pow2')