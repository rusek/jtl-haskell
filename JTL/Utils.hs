module JTL.Utils where

join sep [] = []
join sep [a] = a
join sep (a:b:as) = a ++ sep ++ join sep (b:as)

liftR :: Monad m => (a -> b) -> a -> m b
liftR = (return . )

liftR2 :: Monad m => (a -> b -> c) -> a -> b -> m c
liftR2 f a b = return $ f a b

minWith :: Ord b => (a -> b) -> a -> a -> a
minWith f x y = if f x <= f y then x else y

maxWith :: Ord b => (a -> b) -> a -> a -> a
maxWith f x y = if f x >= f y then x else y

