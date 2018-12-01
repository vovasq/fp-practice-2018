module Task2_2 where
 
import Todo(todo)
import Data.Maybe
 
import Prelude hiding (foldl, foldr,  map, concatMap, unfoldr,
    filter, maxBy, minBy, reverse, sum, product, elem)
 
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl foo ini []     = ini  
foldl foo ini (l:ls) = foldl foo (foo ini l) ls
 
 
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr foo ini []     = ini
foldr foo ini (l:ls) = foo l (foldr foo ini ls)
-- (1 'foo' (2 'foo' (3 'foo' ini)))
 
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr foo ini = unfoldr' (foo ini) where
  unfoldr' (Just (x, ini')) = x : unfoldr foo ini'
  unfoldr' Nothing          = []
 
-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst
 
-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse ls = foldl f [] ls 
  where f t h = h:t
 
-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map foo lst =
 foldr (mapper foo) [] lst where mapper foo x y = (foo x):y  
 
-- Произведение всех элементов списка
product :: [Integer] -> Integer
product []  = 0
product ls  = foldr (*) 1 ls
 
-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr foo [] where
 foo l ls = case l of Nothing -> ls
                      Just l  -> l:ls
                     
-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal lst = snd(foldr foo ((length lst) - 1, []) lst) where
 foo x (i, s) = (i - 1, x !! i : s)
 
 
-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot predicate ls = foldr (checkIfPred predicate) [] ls where
 checkIfPred predicate l ls = if predicate l then l:ls else ls        
 
-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem el ls = foldr (ifIn el) False ls where
 ifIn el l ls = if el == l then True else ls
 
-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr rangeToFoo from where
 rangeToFoo from = if from < to then Just (from, from + step)
                    else Nothing
  
-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append as [] = as
append as bs = let concater as bs = as : bs in    
 foldr concater as bs
 
-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)

groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr (foo n) lst where
  foo n x  = 
    if length x == 0 then Nothing
      else Just(take (fromIntegral n) x , drop (fromIntegral n) x)