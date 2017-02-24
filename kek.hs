module Kek where

-- Хаятов Олег, А3200
-- igromovbox+teaching@gmail.com
-- условия задания
ploshad :: Double -> Double -> Double -> Double
ploshad _ _ _ = 1
-- Ploshad a b c = sqrt(p*(p-a)*(p-b)*(p-c)) where let p = (a+b+c)/2	

fib :: Integer -> Integer
fib n = fib' n 1 1 1

fib':: Integer -> Integer -> Integer -> Integer -> Integer
fib' n k fk1 fk2 = fib' n (k+1) (fk1+fk2) fk1 