{-# LANGUAGE ExplicitForAll, RankNTypes, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances,
             FunctionalDependencies #-}

module JTL.Sequence (
    Sequence, Fail(..),
    emptyS, singletonS, listS, maybeS, failS, chainS, joinS, foldSM, fold1SM, filterS, pickS, takeSM, unwrapSM,
    truncateS, findSM, searchSM, reverseS, mapSM
    ) where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Data.Maybe (mapMaybe, catMaybes)

newtype Sequence e a = Sequence { runSequence :: forall b. (a -> Sequence e a -> b) -> b -> (e -> b) -> b }

class Fail e r | r -> e where
    runFailCont :: r a -> (a -> b) -> (e -> b) -> b
    runFailCont r cont error = case runFailEither r of
        Left e -> error e
        Right x -> cont x
    runFailEither :: r a -> Either e a
    runFailEither r = runFailCont r Right Left

instance Error e => Fail e (Either e) where
    runFailEither = id

emptyS :: Sequence e a
emptyS = Sequence $ \nonempty empty error -> empty

singletonS :: a -> Sequence e a
singletonS x = Sequence $ \nonempty empty error -> nonempty x emptyS

maybeS :: Maybe a -> Sequence e a
maybeS Nothing = emptyS
maybeS (Just x) = singletonS x

listS :: [a] -> Sequence e a
listS [] = emptyS
listS (x:xs) = Sequence $ \nonempty empty error -> nonempty x $ listS xs

errorS :: e -> Sequence e a
errorS e = Sequence $ \nonempty empty error -> error e

failS :: Fail e r => r (Sequence e a) -> Sequence e a
failS r = runFailCont r id errorS

chainS :: Sequence e a -> Sequence e a -> Sequence e a
chainS s p = Sequence $ \nonempty empty error -> runSequence s
    (\x s' -> nonempty x (s' `chainS` p)) (runSequence p nonempty empty error) error

joinS :: [Sequence e a] -> Sequence e a
joinS = foldl chainS emptyS

foldSM :: MonadError e m => (a -> b -> m a) -> a -> Sequence e b -> m a
foldSM f x s = runSequence s (\x' s' -> f x x' >>= \x -> foldSM f x s') (return x) throwError

fold1SM :: MonadError e m => (a -> a -> m a) -> Sequence e a -> m (Maybe a)
fold1SM f s = runSequence s (\x' s' -> liftM Just $ foldSM f x' s') (return Nothing) throwError

mapSM :: MonadError e m => (a -> m b) -> Sequence e a -> m [b]
mapSM f s = liftM reverse $ foldSM (\ys x -> f x >>= \y -> return (y:ys)) [] s

findSM :: MonadError e m => (a -> m Bool) -> Sequence e a -> m (Maybe a)
findSM f s = runSequence s (\x s' -> f x >>= \b -> if b then return $ Just x else findSM f s')
    (return Nothing) throwError

searchSM :: MonadError e m => Sequence e a -> (a -> m Bool) -> m (Maybe a)
searchSM = flip findSM

filterS :: Fail e r => (a -> r Bool) -> Sequence e a -> Sequence e a
filterS f s = Sequence $ go s where
    go s nonempty empty error = runSequence s (\x s' -> (runFailCont $ f x)
        (\b -> if b then nonempty x (filterS f s') else go s' nonempty empty error)
        error) empty error

pickS :: Fail e r => Sequence e a -> (a -> r Bool) -> Sequence e a
pickS = flip filterS

takeCont :: Int -> Sequence e a -> ([a] -> b) -> (e -> b) -> b -- TODO pozbyć się tego
takeCont n _ cok cerr | n <= 0 = cok []
takeCont n (Sequence s) cok cerr = s (nonempty (n - 1) []) (cok []) cerr where
    nonempty 0 es e s' = cok $ reverse $ e:es
    nonempty n es e s' = runSequence s' (nonempty (n - 1) (e:es)) (cok $ reverse $ e:es) cerr

takeSM :: MonadError e m => Int -> Sequence e a -> m [a]
takeSM n s = takeCont n s return throwError

reverseS :: Sequence e a -> Sequence e a
reverseS s = go [] s where
    go xs s = runSequence s (\x -> go (x:xs)) (listS xs) errorS

truncateS :: Int -> Sequence e a -> Sequence e a
truncateS n _ | n <= 0 = emptyS
truncateS n s = Sequence $ \nonempty empty error -> runSequence s
    (\x s' -> nonempty x $ truncateS (n - 1) s') empty error

unwrapSM :: MonadError e m => Sequence e a -> m [a]
unwrapSM s = go [] s where
    go xs s = runSequence s (\x -> go $ x:xs) (return $ reverse xs) throwError

instance Functor (Sequence e) where
    fmap f s = Sequence $ \nonempty empty error -> runSequence s (\x s' -> nonempty (f x) (fmap f s')) empty error

instance Monad (Sequence e) where
    return = singletonS
    m >>= k = Sequence go where
        go nonempty empty error = runSequence m mnonempty empty error where
            mnonempty a m' = runSequence (k a `chainS` (m' >>= k)) nonempty empty error

instance MonadPlus (Sequence e) where
    mzero = emptyS
    mplus = chainS
