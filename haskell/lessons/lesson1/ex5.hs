import System.Win32 (xBUTTON1)
head_:: [a] -> a
head_ (x:_) = x

tail_ :: [a] -> [a]
tail_ (_: t) = t

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y

main :: IO()
main = do
    print(tail [1, 2, 3])