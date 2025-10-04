data Stack a = Stack [a] deriving Show

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _ = False

push :: a -> Stack a -> Stack a
push x (Stack xs)  = Stack (x:xs)

top :: Stack a -> a
top (Stack (x:_)) = x

pop :: Stack a -> Stack a
pop (Stack []) = Stack []
pop (Stack (x:xs)) = Stack xs

instance Functor Stack where
    fmap _ (Stack []) = Stack []
    fmap f (Stack xs) = Stack (map f xs)

instance Eq a => Eq (Stack a) where
    Stack [] == Stack [] = True
    Stack [] == _ = False
    _ == Stack [] = False
    Stack xs == Stack ys = xs == ys

stack = Stack [1,2,3]
empty_stack = Stack []



main :: IO()
main = do
    print("#4-1: ", push 0 stack)
    print("#4-2: ", isEmpty empty_stack, isEmpty stack)
    print("#4-3: ", top stack)
    print("#4-4: ", pop stack)
    print("#4-5: ", fmap (*2) stack)
    print("#4-6: ", empty_stack == pop (pop (pop stack)),  Stack [1, 2, 3] == stack, Stack [0, 2, 3] == stack)