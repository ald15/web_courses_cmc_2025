sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

minimum' :: (Ord a) => [a] -> a
minimum' l = foldl1 (min) l

maximum' :: (Ord a) => [a] -> a
maximum' l = foldl1 (max) l

map' :: (a -> b) -> [a] -> [b]
map' f l = foldr (\x ac -> f x : ac) [] l

filter' :: (a -> Bool) -> [a] -> [a]
filter' f l = foldr (\x ac -> if f x then x:ac else ac) [] l


main :: IO()
main = do
    print("#1-1: ", sum' [1, 2, 3, 4, 5])
    print("#1-2: ", minimum' [1, 2, 3, 4, -10, 5])
    print("#1-3: ", maximum' [1, 2, 3, 4, -10, 5])
    print("#1-4: ", map' (*2) [1, 2, 3, 4, 5])
    print("#1-5: ", filter' (>3) [1, 2, 3, 4, 5])