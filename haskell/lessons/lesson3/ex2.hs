import Distribution.Simple.Install (install)

data BiTree = Leaf Int | BiTree Int BiTree BiTree
    deriving Show

exampleOfTree1 = (BiTree 1 (Leaf  2) (BiTree 3 (Leaf 4) (Leaf 5)))
exampleOfTree2 = (BiTree 1 (Leaf  2) (BiTree 3 (Leaf 4) (Leaf 5)))
exampleOfTree3 = (BiTree 1 (Leaf  2) (BiTree 3 (Leaf 4) (Leaf 6)))

instance Eq BiTree where
    (Leaf x) == (Leaf y) = x == y
    (BiTree x l1 r1) == (BiTree y l2 r2) = x == y && l1 == l2 && r1 == r2
    _ == _ = False

-- exampleOfTree1 == exampleOfTree2
-- exampleOfTree1 == exampleOfTree3



-- Описание полиморфного типа
-- Тут типом является (PBiTree a)
data PBiTree a = PLeaf a | PBiTree a (PBiTree a) (PBiTree a) 
    deriving Show
instance (Eq a) => Eq  (PBiTree a) where
    (PLeaf x) == (PLeaf y) = x == y
    (PBiTree x l1 r1) == (PBiTree y l2 r2) = x == y && l1 == l2 && r1 == r2
    _ == _ = False
exampleOfTree4 = (PBiTree 1 (PLeaf  2) (PBiTree 3 (PLeaf 4) (PLeaf 6)))


-- map для бинарного полиморфного дерева
mapPBiTree :: (a -> b) -> PBiTree a -> PBiTree b
mapPBiTree f (PLeaf x) = PLeaf (f x)
mapPBiTree f (PBiTree x l r) = PBiTree (f x) (mapPBiTree f l) (mapPBiTree f r)


-- класс-функтор -- конструктор типов полиморфный, но с 1 аргументом типом
class Functor' f where 
    fmap' :: (a -> b) -> f a -> f b
instance Functor' PBiTree where
    fmap' = mapPBiTree

-- fmap' (+10) exampleOfTree4



-- Реализация дерева
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving Show


exampleOfTree5 = (Node 1 (Node 2 Empty (Node 4 Empty Empty)) (Node 3 Empty Empty))


instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)



-- Куст
data KTree a = KNode a [KTree a]

instance Functor KTree where
    fmap f (KNode x l) = KNode (f x) (map (fmap f) l)

-- instance Functor [] where
--     fmap = map


-- Вставка для BST
insert :: (Eq a, Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y l r) | x == y = Node y l r
                    | x < y = Node y (insert x l) r
                    | x > y = Node y l (insert x r)
-- insert 10 exampleOfTree5
-- foldr insert exampleOfTree [12, 13, -1, 10]


-- Сворачиваем дерево поиска в отсортированный список
foldTree ::  (a -> b -> b) -> b -> Tree a -> b
foldTree f ac Empty = ac
foldTree f ac (Node x l r) =  foldTree f (f x (foldTree f ac r)) l
sort' :: (Eq a, Ord a) => [a] -> [a]
sort' = foldTree (:) [] . foldr insert Empty
-- sort' [12, 13, -1, 10]
-- foldTree (+) 0 exampleOfTree5