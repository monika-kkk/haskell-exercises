module FoldlFoldr where

sum' :: (Num a) => [a] -> a
sum' = foldr (+) 0

product' :: (Num a) => [a] -> a
product' = foldr (*) 1
-- product' = foldr1 (*)

and' :: [Bool] -> Bool
and' = foldr (&&) True

or' :: [Bool] -> Bool
or' = foldr (||) False

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

head' :: [a] -> a
head' = foldr (\x _ -> x) undefined

last' :: [a] -> a
last' = foldl (\_ x -> x) undefined

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

list = [4,2,6,5,5,1]

-- foldl - f acc x
-- foldr - f x acc; inf lists
-- (?) Foldable