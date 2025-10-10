import Data.Monoid (Sum(..))

-- #1
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x:xs) | f x = Just x
              | otherwise = find f xs


-- #2
data Maybe' a = Nothing' | Just' a deriving Show
data Ether' a b = Left' a | Right' b deriving Show

instance Eq a => Eq (Maybe' a) where
    (==) :: Maybe' a -> Maybe' a -> Bool
    Nothing'  == Nothing'  = True
    Just' x == Just' y = x == y
    _ == _ = False

instance (Eq a, Eq b) => Eq (Ether' a b) where
    (==) :: Ether' a b -> Ether' a b -> Bool
    Left' a == Left' b = a == b
    Right' a == Right' b = a == b
    _ == _ = False


-- #3
instance Functor Maybe' where
    fmap :: (a -> b) -> Maybe' a -> Maybe' b
    fmap _ Nothing'  = Nothing'
    fmap f (Just' a) = Just' (f a)

instance Functor (Ether' a) where
    fmap :: (b -> c) -> Ether' a b -> Ether' a c
    fmap _ (Left' a) = Left' a
    fmap f (Right' a) = Right' (f a)


-- #4
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

op :: Semigroup a => Tree a -> Tree a -> Tree a
op x Empty = x
op Empty y = y
op (Node x1 l1 r1) (Node x2 l2 r2) = Node (x1 <> x2) (op l1 l2) (op r1 r2)

instance Semigroup a => Semigroup (Tree a) where
  (<>) = op

instance Semigroup a => Monoid (Tree a) where
  mempty  = Empty
  mappend = (<>)

t1 :: Tree (Sum Int)
t1 = Node (Sum 0) (Node (Sum 2) Empty Empty) (Node (Sum 4) Empty Empty)

t2 :: Tree (Sum Int)
t2 = Node (Sum 1) (Node (Sum 3) Empty Empty) Empty


  -- Tests
main :: IO()
main = do
    print("#1-1: ", find (>= 2) [1, 2, 3, 5])
    print("#1-1: ", find (== 10) [10, 0, -10, 10, 25])
    print("#1-1: ", find (> 6) [1, 2, 3, 5])
    print("#2-1: ", Just' 1 == Just' 2)
    print("#2-2: ", Just' 0 == Just' 0)
    print("#2-3: ", Nothing' == Just' 0)
    print("#2-4: ", (Nothing' :: Maybe' Int) == (Nothing' :: Maybe' Int))
    print("#2-5: ", Right' (-1) == Left' (-2))
    print("#2-6: ", (Right' 1 :: Ether' Int Int) == Right' 1)
    print("#2-7: ", (Left' "123" :: Ether' String String) == Right' "1234")
    print("#3-1: ", fmap (*2) (Just' 2))
    print("#3-2: ", fmap (*2) Nothing')
    print("#3-3: ", fmap (*2) (Left' 2))
    print("#3-2: ", fmap (*2) (Right' 2 :: Ether' Int Int))
    print("#4-1: ", op t1 t2)