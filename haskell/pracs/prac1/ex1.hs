change :: [a] -> [a]
change l = if (length l < 2) 
            then l
            else reverse (take 2 l) ++ change (drop 2 l)


main :: IO()
main = do
    print(change [1,2,3,4,5])
    print(change ['a'..'f'])