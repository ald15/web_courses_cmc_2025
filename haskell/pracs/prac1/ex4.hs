replicate' :: Int -> a -> [a]
replicate' n elem = take n (repeat elem)


main :: IO()
main = do
    print(replicate' 5 'm')