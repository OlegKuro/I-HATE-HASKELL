--Хаятов Олег, А3201

--Задание:
{-
Написать функцию listWords :: Dictionary -> [String], которая выдает список всех слов в заданном словаре.
-}
{-
Например, словарь, содержащий слова “bit”, “byte”, “bite”, “site” будет представлен следующим образом (с точностью до порядка элементов списков):
[Node 'b' [Node 'i' [Node 't' [Empty,
                               Node 'e' [Empty]]],
           Node 'y' [Node 't' [Node 'e' [Empty]]]],
 Node 's' [Node 'i' [Node 't' [Node 'e' [Empty]]]]]
 -}

 {-Решение:
 Довольно тривиально. Мы получаем на вход Бора. Дальше делаем обход в глубину, где в каждой вершине
 Бора нужно добавить текущую литеру в вывод.
 -}
module HW5 where
import Data.Char

data Trie = Empty | Node Char [Trie]
type Dictionary = [Trie]

--Главная ф-ия
listWords :: Dictionary -> [String]
listWords =  foldr (++) acc . map (listWords' "") where acc = []

--Обход в глубину Бора и вывод в аккумулятор (acc)
listWords' :: String -> Trie -> [String]
listWords' prev Empty = (\x -> [x]) prev  -- Хотя тут уместнее вывести ошибку с указанием на то, что словарь пустой
listWords' prev (Node x xs) = listWords'' (listWords' $ prev ++ rest) xs where rest = [x]

--"Склеивающая" ф-ия
listWords'' :: (a -> [b]) -> [a] -> [b]
listWords'' f = foldr ((++) . f) []

main = [listWords [Node 'b' [Node 'i' [Node 't' [Empty, Node 'e' [Empty]]], Node 'y' [Node 't' [Node 'e' [Empty]]]],  Node 's' [Node 'i' [Node 't' [Node 'e' [Empty]]]]] == ["bit","bite","byte","site"],
        listWords [Node 't' [Node 'r' [Node 'i' [Node 'p' [Empty]]]]] == ["trip"],
        listWords [Empty, Node 'd' [Empty, Node 'r' [Empty, Node 'u' [Empty, Node 'g' [Empty, Node 's' [Empty]]]]]] == ["", "d", "dr", "dru", "drug", "drugs"],
        listWords [Empty] == [""]]