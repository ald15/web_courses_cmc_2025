-- Урок #3

-- type String [Char] -- опеределение синонима для уже имеющегося типа
-- data Bool = False | True - конструктор данных (для введения нового типа данных)

-- Haskell сам умеет выводить типы
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun 
    deriving (Ord, Show, Read, Bounded, Enum) 
-- Enum - перечислимый тип с 0
-- Bounded - ограниченный тип (есть min, max)
-- Read - тип поддерживает ввод с клавиатуры
-- Show - тип поддерживает преобразование в строку
-- Ord - упорядоченный тип
-- Eq - тип поддерживает сравнение на равенство, const1 = const1, const1 != const2
-- Подключение к типу вручную
instance Eq Day where 
    Mon == Mon = True
    Tue == Tue = True
    Wed == Wed = True
    Thu == Thu = True
    Fri == Fri = True
    Sat == Sat = True
    Sun == Sun = True
    _ == _ = False