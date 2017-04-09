module LC2 where

data BTree t = Empty | Root (BTree t) t (BTree)
instance Functor BTree where
	fmap _ Empty = Empty
	fmap f (Root t1 root t2) = Root (fmap f t1) (f root) (fmap f t2)

-- fmap (+1) tree | - теперь каждый элемент дерева увеличится на 1

data Either a b = Left a | Right b
instance Functor (Either c) where
	fmap _ x@(Left _) = x
	fmap f (Right x) = Right (f x)
index :: Int -> [a] -> Either String a
index n list 
	| n<0 || n>= length list = Left("Wrong index " ++ show n)
	| otherwise = Right (list !! n)