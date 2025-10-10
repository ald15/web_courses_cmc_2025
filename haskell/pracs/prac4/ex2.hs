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


main :: IO()
main = do
    print("#2-1: ", Just' 1 == Just' 2)
    print("#2-2: ", Just' 0 == Just' 0)
    print("#2-3: ", Nothing' == Just' 0)
    print("#2-4: ", (Nothing' :: Maybe' Int) == (Nothing' :: Maybe' Int))
    print("#2-5: ", Right' (-1) == Left' (-2))
    print("#2-6: ", (Right' 1 :: Ether' Int Int) == Right' 1)
    print("#2-7: ", (Left' "123" :: Ether' String String) == Right' "1234")