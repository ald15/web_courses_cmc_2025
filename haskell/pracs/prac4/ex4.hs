import Data.Monoid (Sum(..))

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


main :: IO()
main = do
    print("#4-1: ", op t1 t2)