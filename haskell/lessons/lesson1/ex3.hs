sign :: (Ord a, Num a) => a -> a
sign n | n < 0 = -1
       | n == 0 = 0
       | otherwise = 1

main :: IO()
main = do
    print(sign (-6))