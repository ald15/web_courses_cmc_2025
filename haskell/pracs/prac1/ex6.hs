zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] l2 = []
zipWith' f l1 [] = []
zipWith' f l1 l2 = [(head l1) `f` (head l2)] ++ zipWith' f (tail l1) (tail l2)


main :: IO()
main = do
    print(zipWith' (*) [2,3,4] [6,7,8,9])