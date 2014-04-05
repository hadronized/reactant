{-# LANGUAGE DeriveFunctor, DeriveFoldable, MultiParamTypeClasses
           , FlexibleInstances, GeneralizedNewtypeDeriving #-}

module FRP.Reactant where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Reader
import Data.Monoid

-- |A time-varying value.
newtype Reactive t a = Reactive (t -> a) deriving (Functor, Applicative)

-- |An events stream.
newtype Event t a = Event [(t,a)] deriving (Functor, Monoid)

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

-- |Concat a monoidal events stream.
mconcatE :: (Monoid a) => Event t a -> Event t a
mconcatE (Event e) = Event $ foldr f [] e
  where
    f x       []        = [x]
    f (lt,a) ((_,b):xs) = xs ++ [(lt,b <> a)]

-- |Filter an events stream, only saving those who satisfy the predicate.
filterE :: (a -> Bool) -> Event t a -> Event t a
filterE pred (Event e) = Event (filter (pred . snd) e)

-- |Accumulate a value in an events stream.
accumE :: a -> Event t (a -> a) -> Event t a
accumE i e = fmap ($ i) e

-- |Build a time-varying value from an events stream.
reactive :: (Ord t) => Event t a -> Reactive t a
reactive (Event e) =
    Reactive $ \t ->
      let lastE = last e
      in if t >= fst lastE then
        snd lastE
        else
          snd . head . dropWhile (\(t0,_) -> t <= t0) $ e

-- |A reactant. It’s basically a monad that embeds time generation.
--
-- Minimal definition: `now` and `trigger`.
class (Monad m) => MonadReactant t m where
  -- |
  now :: m t
  -- |
  at :: Reactive t a -> t -> m a
  at (Reactive r) t = return $ r t
  -- |
  trigger :: a -> m (Event t a)
  -- |
  triggers :: [a] -> m (Event t a)
  triggers t = mconcat `liftM` mapM trigger t

-- |A pure reactant.
newtype Reactant t a = Reactant {
    unReactant :: State t a
  } deriving (Monad)

instance (Enum t) => MonadReactant t (Reactant t) where
  now       = Reactant get
  trigger a = Reactant . state $ \t -> (Event [(t,a)],succ t)

-- |Run a pure reactant with a an initial time. If your time is in the class
-- `Num`, you may want to pass `0` as initial value.
runReactant :: t -> Reactant t a -> a
runReactant start r = evalState (unReactant r) start

-- |A reactant in `IO`.
newtype ReactantIO t a = ReactantIO {
    unReactantIO :: ReaderT (TVar t) IO a
  } deriving (Monad,MonadIO)

instance (Enum t) => MonadReactant t (ReactantIO t) where
  now = ReactantIO $ ask >>= lift . atomically . readTVar
  trigger a = ReactantIO $ do
    g <- ask
    t <- lift . atomically $ do
      t <- readTVar g
      writeTVar g (succ t)
      return t
    return $ Event [(t,a)]

-- |Run reactant from IO with a an initial time. If your time is in the class
-- `Num`, you may want to pass `0` as initial value.
runReactantIO :: t -> ReactantIO t a -> IO a
runReactantIO start r =
    atomically (newTVar start) >>= runReaderT (unReactantIO r)

-- should be place in tests/
test :: ReactantIO Int ()
test = do
  e0 <- trigger (Endo $ (+2))
  e1 <- trigger (Endo $ (*3))
  let e = accumE 0 . fmap appEndo . mconcatE $ e0 <> e1 :: Event Int Int
      r = reactive e
  t <- now
  v <- r `at` t
  liftIO . putStrLn $ "value is " ++ show v
