-- #1
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

instance Eq a => Eq (Tree a) where
    Empty == Empty = True
    Empty == _ = False
    _ == Empty = False
    (Node x1 l1 r1) == (Node x2 l2 r2) = 
        x1 == x2 && l1 == l2 && r1 == r2
    
tree10 = Node 5 (Node 7 Empty Empty) Empty
tree11 = Node 5 (Node 7 Empty Empty) Empty
tree12 = Node 6 Empty Empty


-- #2
addTrees :: Num a => Tree a -> Tree a -> Tree a
addTrees Empty Empty = Empty
addTrees Empty tree = tree
addTrees tree Empty = tree
addTrees (Node x l1 r1) (Node y l2 r2) = Node (x + y) (addTrees l1 l2) (addTrees r1 r2)

tree21 = Node 1 (Node 2 Empty (Node 5 Empty Empty)) (Node 3 Empty Empty)
tree22 = Node 10 (Node 7 (Node 8 Empty Empty) Empty) (Node 4 Empty Empty)
tree23 = Node 11 (Node 9 (Node 8 Empty Empty) (Node 5 Empty Empty)) (Node 7 Empty Empty)


-- #3
mapTreeToTree :: Tree (a -> b) -> Tree a -> Tree b
mapTreeToTree Empty _ = Empty
mapTreeToTree _ Empty = Empty
mapTreeToTree (Node f lf rf) (Node x lx rx) = Node (f x) (mapTreeToTree lf lx) (mapTreeToTree rf rx)

tree31 = Node (+1) (Node (*2) (Node (+5) Empty Empty) Empty) (Node (\x -> x - 3) Empty Empty)
tree32 = Node 10 (Node 7 (Node 8 Empty Empty) Empty) (Node 4 Empty Empty)
tree33 = Node 11 (Node 14 (Node 13 Empty Empty) Empty) (Node 1 Empty Empty)

main :: IO()
main = do
    print("#1-0: ", tree10 == tree11)
    print("#1-1: ", tree11 == tree12)
    print("#1-2: ", tree10 == tree12)
    print("#2-1: ", addTrees tree21 tree22 == tree23)
    print("#3-1: ", mapTreeToTree tree31 tree32 == tree33)