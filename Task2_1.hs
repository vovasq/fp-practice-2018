module Task2_1 where
 
import Todo(todo)
 
import Prelude hiding (lookup)
 

tree = treeFromList [(7, 93), (1,99), (2,98), (3, 97), (8, 92), (5,95), (6, 94), (4,96)]

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
 
data TreeMap v = Empty | Node Integer v (TreeMap v) (TreeMap v)
  deriving (Show, Eq)
 
-- Пустое дерево
empty :: TreeMap v
empty = Empty

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains Empty _ = False
contains (Node key _ left right) k
  | k == key = True
  | k < key  = contains left k
  | k > key  = contains right k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup _ Empty = error "No such an element or Tree is Empty"
lookup k (Node key value left right)
  | k < key  = lookup k left
  | k > key  = lookup k right
  | k == key = value  
 
-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (newKey, newValue) Empty = Node newKey newValue Empty Empty
insert (newKey, newValue) (Node key value left right)
  | newKey < key  = Node key value (insert (newKey, newValue) left) right
  | newKey > key  = Node key value left (insert (newKey, newValue) right)
  | newKey == key = error "Already contains this element"
 
-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove _ Empty = Empty
remove i (Node key value left right)
  | i < key = Node key value (remove i left) right
  | i > key = Node key value left (remove i right)
  | otherwise = case (left, right) of
    (Empty, Empty) -> Empty
    (left, Empty)  -> left
    (Empty, right) -> right
    (left, right)  -> mergeTrees left right
    where 
      mergeTrees l Empty = l
      mergeTrees l (Node key value Empty right) = Node key value l right
      mergeTrees l (Node key value left right) = Node key value (mergeTrees l left) right
 
-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE _ Empty = error "No such an element"
nearestLE i (Node key value left right)
  | key > i = nearestLE i left
  | key < i  = case right of
    (Node key value _ _) | (i == key) -> (key, value)
    (Node key value _ _) | (i /= key) -> nearestLE i right
    otherwise -> (key, value)
  | otherwise = (key, value)

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert Empty lst
 
-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree Empty                       =  []
listFromTree (Node key value left right) = listFromTree(left) ++ [(key, value)] ++ listFromTree(right)

size :: TreeMap v -> Integer
size Empty = 0
size (Node _ _ left right) = 1 + (size left)  + (size right)

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ Empty = error "Tree is empty"
kMean i (Node key value left right)
  | (size left) == i = (key, value)
  | (size left) > i  = kMean i left
  | otherwise     = kMean (i - (size left) - 1) right
 
