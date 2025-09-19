-- #1
change :: [a] -> [a]
change l = if (length l < 2) 
            then l
            else reverse (take 2 l) ++ change (drop 2 l)


-- #2
reverse' :: [a] -> [a]
reverse' [] = []
reverse' l = [last l] ++ reverse' (init l)


-- #3
elem' :: (Eq a) => a -> [a] -> Bool
elem' e [] = False
elem' e l = if ((head l) == e)
            then True
            else elem' e (tail l)


-- #4
replicate' :: Int -> a -> [a]
replicate' n e = take n (repeat e)


-- #5
zip' :: [a] -> [b] -> [(a, b)]
zip' [] l2 = []
zip' l1 [] = []
zip' l1 l2 = [((head l1), (head l2))] ++ zip' (tail l1) (tail l2)


-- #6
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] l2 = []
zipWith' f l1 [] = []
zipWith' f l1 l2 = [(head l1) `f` (head l2)] ++ zipWith' f (tail l1) (tail l2)


-- #7
eqList :: (Eq a) => [a] -> [a] -> Bool
eqList [] [] = True
eqList l1 l2 = if ((length l1) /= (length l2))
                then False
                else 
                    if ((head l1) /= (head l2))
                    then False
                    else eqList (tail l1) (tail l2)


main :: IO()
main = do
        print("#1:", change [1,2,3,4,5])
        print("#2:", reverse' [5, 4, 3, 2, 1])
        print("#3:", elem' 7 [1,3,5,7,9,11])
        print("#4:", replicate' 5 'm')
        print("#5:", zip' [1,2,3,4] ['a','b','c'])
        print("#6:", zipWith' (*) [2,3,4] [6,7,8,9])
        print("#7:",eqList "abce" "abcd")
