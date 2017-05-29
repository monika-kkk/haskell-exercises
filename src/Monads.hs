module Monads where

inc :: Maybe Int -> Maybe Int
inc v = do
        x <- v
        return x

data Box a = Empty | Box a deriving Show

instance Functor Box where
    fmap _ Empty = Empty
    fmap f (Box a) = Box $ f a

instance Applicative Box where
    pure a = Box a
    Empty <*> _ = Empty
    (Box f) <*> a = fmap f a

instance Monad Box where
    return = pure
    Empty >>= _ = Empty
    Box a >>= f = f a

v = Box 2 :: Box Int

func :: Int -> Box String
func v = Box $ show $ v * 3

