{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}

module FRP.Reactant where

import Control.Applicative
import Control.Monad
import Data.Monoid

newtype Reactive e a = Reactive { unReactive :: e -> a } deriving (Functor,Applicative,Monad)

newtype Event e a = Event { runEvent :: e -> Maybe a } deriving (Functor)

instance Applicative (Event e) where
  pure = Event . const . Just
  Event a <*> Event f = Event $ \e -> a e <*> f e

instance Monad (Event e) where
  return = pure
  Event a >>= f =
      Event $ \e -> case a e of
        Just x  -> runEvent (f x) e
        Nothing -> Nothing

instance MonadPlus (Event e) where
  mzero = mempty
  mplus = mappend

instance Monoid (Event e a) where
  mempty = Event (const Nothing)
  Event e0 `mappend` Event e1 = Event $ \e -> e0 e <|> e1 e

never :: Event e a
never = mempty

always :: a -> Event e a
always = pure

reactive :: a -> Event e a -> Reactive e a
reactive a (Event ev) = Reactive $ \e -> maybe a id (ev e)

untilR :: Reactive e a -> Event e (Reactive e a) -> Reactive e a
untilR (Reactive initial) (Event sw) =
    Reactive $ \e -> case sw e of
      Just new -> unReactive new e
      Nothing  -> initial e

