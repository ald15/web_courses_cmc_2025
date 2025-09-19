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
    print(eqList [1, 2, 3, 4 ,5] [1, 2, 3, 4, 5])
    print(eqList "abce" "abcd")
    print(eqList [] [1])