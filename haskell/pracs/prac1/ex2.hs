reverse' :: [a] -> [a]
reverse' [] = []
reverse' l = [last l] ++ reverse' (init l)


main :: IO()
main = do
        print(reverse' [5, 4, 3, 2, 1])