zip' :: [a] -> [b] -> [(a, b)]
zip' [] l2 = []
zip' l1 [] = []
zip' l1 l2 = [((head l1), (head l2))] ++ zip' (tail l1) (tail l2)


main :: IO()
main = do
    print(zip' [1,2,3,4] ['a','b','c'])