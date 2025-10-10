data Maybe' a = Nothing' | Just' a deriving Show
data Ether' a b = Left' a | Right' b deriving Show

instance Functor Maybe' where
    fmap :: (a -> b) -> Maybe' a -> Maybe' b
    fmap _ Nothing'  = Nothing'
    fmap f (Just' a) = Just' (f a)

instance Functor (Ether' a) where
    fmap :: (b -> c) -> Ether' a b -> Ether' a c
    fmap _ (Left' a) = Left' a
    fmap f (Right' a) = Right' (f a)


main :: IO()
main = do
    print("#3-1: ", fmap (*2) (Just' 2))
    print("#3-2: ", fmap (*2) Nothing')
    print("#3-3: ", fmap (*2) (Left' 2))
    print("#3-2: ", fmap (*2) (Right' 2 :: Ether' Int Int))