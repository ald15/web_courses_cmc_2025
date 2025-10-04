-- Maybe - когда нужно выводить что-то при отсутвии элемента данного типа
-- data Maybe a = Nothing | Just a
-- find :: (a -> Bool) -> [a] -> Maybe a


-- data Either a b = Left a | Right b
divide' :: Int -> Int -> Either String Int
divide' x y | y == 0 = Left "Error: divide by zero!"
            | otherwise = Right (div x y)

-- instance Functor (Either a)
--     where ...


-- Класс Applicative — это один из классов типов, который используется для обработки контекстуальных значений,
-- таких как значения, обернутые в структуры данных типа Maybe, List, IO и другие.
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

-- [TODO] Почему-то не работает, разобраться!
-- instance Applicative Maybe where
--     pure = Just
--     (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
--     Nothing <*> _  = Nothing 
--     _ <*> Nothing = Nothing
--     (Just f) <*> (Just x) = Just (f x)

-- Just (*2) <*> Just 5
-- Just 10


-- Что это? - Разобраться [TODO]
-- class Monoid t where 
--     mempty :: t
--     mappend :: t -> t -> t

-- (a <> b) <> c = a <> (b <> c) = Nothing
-- e <> x = x <> e = x 

-- + 0
-- * 1
-- ++ []
-- && True
-- || False

-- instance Monoid [a] where
--     mempty = []
--     mappend = (++)


-- id - тождественная функция, примиенима к композиции .


-- Кто вы войны?) - [TODO]
newtype All = All Bool
newtype Any = Any Bool
newtype Sum a = Sum a
newtype Product a = Product a
newtype Endo a = Endo a







