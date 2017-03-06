--Хаятов Олег, А3201

--Задание:
{-
В заданной строке символов будем считать числом произвольную последовательность цифр, слева и справа от которой
не находится цифра. Написать функцию maxNumber :: String -> Integer, выдающую числовое значение самого большого 
“числа” в строке. Например, результатом вызова функции с аргументом "0xFF55 00012 -100 19" должно быть 100. Если 
чисел в строке нет совсем, то это ошибка аргумента.

-}

{-
Решение
Пишем регулярку, которая разобьёт строку на список строк, состоящих только из подряд идущих цифр
Потом к каждому элементу этого списка применяем createNumber - самописный парсер чисел
Далее просто при помощи тривиальной рекурсии находим максимум
Некоторые сложности: вспомнить, что существуют регулярные выражения и автоматы, вспомнить синтаксис мапа, а не писать 
всё отдельной функцией, как я пытался сделать.
Ну и еще не хотелось самому писать парсер, но ничего умнее я на Hoogle не нашел, да и места вроде не много занимает,
так что сойдёт.
-}
module HW2_2 where
import Text.Regex (splitRegex, mkRegex)
import Data.Char

--функция, с кот. всё начинается
maxNumber :: String -> Integer
maxNumber str 
	|(null str) = error "Please input something"
	|otherwise = ( (if not(null a) then maxNumber' (map createNumber a) 0 else error "Argument error") ) where a = splitOnNumbers str

--получает список чиселок, находит максимальное (по дефолту максимум - это ноль,
-- так как в контексте задания нельзя получить отрицательное число)
maxNumber' :: [Integer] -> Integer -> Integer
maxNumber' (x:xs) prev
	|((null xs) && (x>prev)) = x
	|(null xs) = prev
	|x > prev = maxNumber' xs x
	|x <= prev = maxNumber' xs prev

--регэксп, делит строку на массив строк из цифр
splitOnNumbers :: String -> [String]
splitOnNumbers list = filter (not . null) (a list) where a = splitRegex(mkRegex "[^0-9]+")

--тут и вылезет ошибка, если нечего парсить (она же ошибка аргумента)
createNumber :: String -> Integer
createNumber str
	|(null str) = error "Nothing to parse"
	|otherwise = createNumber' str 0

--работаем в десятичной системе счисления, парсим всё в ручную
--assert: not empty arg
createNumber' :: String -> Integer -> Integer
createNumber' (x:xs) prev
	|(null xs) = prev*10 + parse x
	|otherwise = createNumber' xs (prev*10 + parse x)

--тривиальщина, но вроде как нужна)))
parse :: Char -> Integer
parse '0' = 0
parse '1' = 1 
parse '2' = 2 
parse '3' = 3 
parse '4' = 4 
parse '5' = 5 
parse '6' = 6 
parse '7' = 7 
parse '8' = 8 
parse '9' = 9 

main=[maxNumber "12asw1sad232133sa 12312" == 232133,maxNumber "0xFF55 00012 -100 19" == 100, maxNumber"asdasdqwe" == 0]