elem' :: (Eq a) => a -> [a] -> Bool
elem' e [] = False
elem' e l = if ((head l) == e)
            then True
            else elem' e (tail l)


main :: IO()
main = do
    print(elem' 7 [1,3,5,7,9,11])
