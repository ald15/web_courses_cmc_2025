twice_ :: (a -> a) -> a -> a
twice_ f x = f (f x)

len_ :: [a] -> Int
len_ [] = 0
len_ (_ : t) = 1 + len_ t

take_ :: Int -> [a] -> [a]
take_ 0 _ = []
take_ _ [] = []
take_ n (x:xs) = x : take_ (n-1) xs


drop_ :: Int -> [a] -> [a]
drop_ 0 x = x
drop_ _ [] = []
drop_ n (_ : xs) = drop_ (n-1) xs

(+++) :: [a] -> [a] -> [a]
[] +++ l = l
(x:xs) +++ y = x:(xs+++y)


(!!!):: [a] -> Int -> a
(x:_) !!! 0 = x
(x:xs) !!! n = xs !!! (n - 1)

-- Alternative

-- (!!!) n l = head(drop (n-1) l)


fromTo::(Enum a, Eq  a) => a -> a -> [a]
fromTo x y = if x == y 
        then [x] 
        else x:fromTo (succ x) y

main :: IO()
main = do
    print(take_ 2 [1, 2, 3, 4, 5])
    print([1, 2, 3] +++ [4, 5, 6])
    print(fromTo 'a' 'z')