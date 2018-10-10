module Task1_2 where

import Todo(todo)
import Data.Fixed
import Prelude hiding (sin, cos, gcd)

-- синус числа (формула Тейлора)
-- sin x = (-1) ^ n * x ^ (2 * n + 1) / (2 * n + 1)!  
-- sin x = x - x ** 3 / (2 * 3) + x ** 5 / (5!)

denomSin :: Double -> Double
denomSin n = (2 * n * (2 * n + 1)) 

sinus :: Double -> Double -> Double -> Double
sinus x x_i n = 
  let precision = 0.0000001 
      x_i_1 = x_i * x * x * (-1) / (denomSin n) in
      if (abs x_i) > precision then 
        x_i + (sinus x x_i_1 (n + 1))
      else 0.0

sin :: Double -> Double
sin x =
  let n = 1 in
    sinus x x n 

denomCos :: Double -> Double
denomCos n = (2 * n - 1) * 2 * n 

cosinus :: Double -> Double -> Double -> Double
cosinus x x_i n = 
  let precision = 0.0000001 
      x_i_1 = x_i * x * x * (-1) / (denomCos n) in
      if (abs x_i) > precision then 
        x_i + (cosinus x x_i_1 (n + 1))
      else 0.0

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = 
  let n = 1 in 
    cosinus x 1 n

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y = 
  if x == 0 then y else
    if y == 0 then x else
      if x > y then gcd (mod x y) y
      else gcd x (mod y x)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to 
  | from == to    = False
  | mod' x 1 == 0 = True 
  | otherwise = doesSquareBetweenExist (from + 1) to
  where x = sqrt (fromIntegral from)



-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y = todo

isPrimeRecurs :: Integer -> Integer -> Bool
isPrimeRecurs n i = 
  if i * i < n then
    if (mod n i) == 0 then False
    else isPrimeRecurs n (i + 2)
  else True

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 2 = True
isPrime 3 = True
isPrime n = 
  if (mod n 6) == 1 || (mod n 6) == 5 then
    isPrimeRecurs n 5
  else
    False


type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
-- getCosFromScalarProd :: Point2D -> Point2D -> Double
-- getCosFromScalarProd a b =
--   let  

triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
