--Хаятов Олег, А3201

--Задание:
{-
Задание
Написать функцию preHigher :: Ord a => [a] -> [Int], которая по заданному
списку элементов выдаёт список индексов тех из них, которые строго меньше
следующего. Например, в числовом списке [1, 2.2, 2.1, 3.14, 2.7, 1.618] 
числа, меньшие следующего в списке - это {1} и {2.1}, поэтому вызов preHigher
[1, 2.2, 2.1, 3.14, 2.7, 1.618] должен выдать список [0, 2] - список индексов
этих элементов.
-}

{-
Решение:
Будем решать задачу, создав что-то типа массива элементов такого вида:
[cur, next, ord]. Что есть что? cur - текущий элемент, next - следующий (получим сдвигом массива влево на 1),  ord - порядковый номер,
он же индекс, он же наша цель.
Почему именно так?
1)Сконструировав такие "тройки" можно будет и применить функции высших порядков (map, filter)) и сделать код менее нагруженным и человеко-понятным
2)Кажется, это расходует 3*n памяти, что есть O(n) да и решает всё за линию.
To sum up: решение неплохое, можно было написать иначе (например, завернуть всё в рекурсию и отделять 2 элемента из головы (x:y:xs) e.t.c), но
конкретно в этом задании нужно расчехлить функции высших порядков, чем я тут собсна и занимаюсь :D

Детали реализации и некоторые душещипательные моменты:
1) я буду дико гореть, если окажется, что есть аналог zip для троек
2) было больно дебажить
	2.1) сперва как даун решал не ту задачу (текущий элемент сравнивал с предыдущим, из-за чего была строка:
		(preHigher' (customZipper list ([0] ++ list) ([0 .. (length list) - 1]) [])))
		компилятор плевался в меня и говорил, что сигнатура главной функции не та, а я гнуснейший человек
		сделал [0.0] ++ list 
		ghci решил вспомнить про Fractional, хотя я имел в виду совсем не его

	2.2) никак не мог завернуть правильно рекурсию и долго думал, можно ли сверткой сделать тройки.
		в итоге нашел упоминание о том, что в fold* можно пихать бинарные/унарные функции, в общем случае более простые,
		но как бы я не ухитрялся, не смог бы получить список троек
3) выяснил, что Prelude> [100..10] выводит []
не понял, почему так и почему разрабы не запарились это сделать. расстроился. Т_Т

-}
module HW4 where

-- assert: length list > 0
preHigher :: Ord a => [a] -> [Int]
preHigher [] = []
preHigher list = (preHigher' (customZipper list (tail list) ([0 .. (length list) - 1]) []))

-- фильтр чекает элементы, согласно нашему заданию, затем мап выводит индекс, он же 3-ий элемент в тройке 
preHigher' :: Ord a => [(a,a,Int)] -> [Int]  
preHigher' list = map (\(f,s,t) -> t) (filter (\(b, c, d) -> b < c) (list))

--наш самописный зиппер с хвостовой (вроде) рекурсией
customZipper :: Ord a => [a] -> [a] -> [Int] -> [(a,a,Int)] -> [(a,a,Int)]
customZipper (fx:fxs) (sx:sxs) (tx:txs) prev
	|not (null fxs || null sxs || null txs) = (prev ++ [(fx,sx,tx)]) ++ customZipper fxs sxs txs prev
	|otherwise = prev ++ [(fx,sx,tx)]

--тестики (последовательности чисел генерировал не я сам, а сайтик http://randstuff.ru/number/)
main = [preHigher [11, 10, 26, 56, 28, 72, 93] == [1,2,4,5], preHigher [1, 2.2, 2.1, 3.14, 2.7, 1.618] == [0,2], preHigher [0..50] == [0..49],preHigher [100,99,98] == []]