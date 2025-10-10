-- #1
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x:xs) | f x = Just x
              | otherwise = find f xs


main :: IO()
main = do
    print("#1-1: ", find (>= 2) [1, 2, 3, 5])
    print("#1-1: ", find (== 10) [10, 0, -10, 10, 25])
    print("#1-1: ", find (> 6) [1, 2, 3, 5])