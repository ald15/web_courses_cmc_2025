-- #4
path :: [(Float, Float)] -> Float
path l = sum (zipWith d2dots l ( l))

d2dots :: (Float, Float) -> (Float, Float) -> Float
d2dots (a, b) (c, d) = sqrt ((a-c)^2 + (b-d)^2)


main :: IO()
main = do
    -- Случайный набор
    print("#4-1: ", path [(1, 2), (3, 4), (5, 6), (7, 8), (9, 10)])
    -- Периметр прямоугольника со сторонами 3 и 4
    print("#4-2: ", path [(0, 0), (0, 3), (4, 3), (4, 0), (0, 0)])