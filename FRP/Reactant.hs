{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module FRP.Reactant where

import Control.Applicative
import Control.Monad
import Data.Monoid

-- |
newtype Reactive t a = Reactive (t -> a)

instance Functor (Reactive t) where
  fmap f (Reactive g) = Reactive (f . g)

instance Applicative (Reactive t) where
  pure = Reactive . const
  Reactive f <*> Reactive g = Reactive $ \t -> f t (g t)

-- |
newtype Event t a = Event [(t,a)]

instance Functor (Event t) where
  fmap f (Event e) = Event . map (fmap f) $ e

-- |Event that never occurs.
never :: Event t a
never = Event []

-- |Merge two events streams.
merge :: (Ord t) => Event t a -> Event t a -> Event t a
merge (Event e0) (Event e1) = Event $ mergeList e0 e1
  where
    mergeList a  [] = a
    mergeList [] b = b
    mergeList a@((t0,x):xs) b@((t1,y):ys)
      | t0 <= t1  = (t0,x) : mergeList xs b
      | otherwise = (t1,y) : mergeList a ys

-- |Filter an events stream, only saving those who satisfy the predicate.
filterE :: (a -> Bool) -> Event t a -> Event t a
filterE pred (Event e) = Event $ filter (pred . snd) e

-- |Accumulate a value in an events stream.
accumE :: a -> Event t (a -> a) -> Event t a
accumE i e = fmap ($ i) e

-- |
reactive :: (Ord t) => Event t a -> Reactive t a
reactive (Event e) =
    Reactive $ \t ->
      let lastE = last e
      in if t >= fst lastE then
        snd lastE
        else
          snd . head . dropWhile (\(t0,_) -> t <= t0) $ e

-- |
class (Monad m) => MonadReactant m t where
  -- |
  trigger :: a -> m (Event t a)
  -- |
  triggers ::[a] -> m (Event t a)
  triggers t = foldM fastMerge never $ mapM trigger t
    where
      fastMerge (Event a) (Event x) = return $ Event (a ++ x)

-- |
newtype Reactant t a = Reactant { runReactant :: t -> (a,t) }

instance Monad (Reactant t) where
  return a = Reactant $ \t -> (a,t)
  r >>= f = Reactant $ \t ->
    let (a,t0) = runReactant r t
    in runReactant (f a) t0

instance (Ord t, Enum t) => MonadReactant (Reactant t) t where
  trigger a = Reactant $ \t -> (Event [(t,a)],succ t)
