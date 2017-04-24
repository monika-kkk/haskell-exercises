module Lab1 where

head' (h:_)  = h

length' []  = 0
length' (_:t) = 1 + length' t

take' _ [] = []
take' 0 _ = []
take' n (h:t) = h : take' (n-1) t

map' _ [] = []
map' f (h:t) = f h : map' f t

(+++) [] l = l
(+++) l [] = l
(+++) x y = (+++) (init x) (last x : y)