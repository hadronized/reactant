-- |@Rea t a@ is a /reactive/ 'a' value that may react to time 't'. If it
-- doesn’t react to time, it’s considered as /inhibiting/ – see 'dead'.
--
-- Inhibiting is used when a reactive value is not responding to time anymore.
-- You can make it respond again by *reviving* it – see 'revive' or '(~>)'.
--
-- 'Event t a' represents an /event stream/. Each event has an occurrence in
-- time and can be reacted to. The simplest kind of reaction is to use them as
-- behavior switches. You use a 'Rea t a' reactive value and switch to another
-- 'Rea t a' reactive value when the given event occurs – see 'till'.

module FRP.Reactant where

import Control.Applicative
import Control.Arrow ( Arrow(..) )
import Control.Category ( Category(..) )
import Data.Monoid ( Monoid(..) )
import Data.Profunctor ( Profunctor(..) )
import Data.Semigroup ( Semigroup(..) )
import Prelude hiding ( (.), id )

-- |@Rea t a@ is a /reactive 'a' value that may react to time 't'.
newtype Rea t a = Rea { stepRea :: t -> Maybe (a,Rea t a) } deriving (Functor)

instance Category Rea where
  id = now
  a . b = Rea $ \t -> do
    (t',bn) <- stepRea b t
    (ax,an) <- stepRea a t'
    return (ax,an . bn)

instance Arrow Rea where
  arr f = let r = Rea $ \t -> return (f t,r) in r
  first f = recRea
    where
      recRea = Rea $ \(t,d) -> do
        (fx,fn) <- stepRea f t
        return ((fx,d),recRea)

instance Profunctor Rea where
  dimap f g r = Rea $ \t -> do
    (x,n) <- stepRea r (f t)
    return (g x,dimap f g n)

instance Applicative (Rea t) where
  pure = still
  f <*> x = Rea $ \t -> do
    (f',nf) <- stepRea f t
    (x',nx) <- stepRea x t
    return $ (f' x',nf <*> nx)

instance Semigroup (Rea t a) where
  a <> b = Rea $ \t -> case stepRea a t of
    Just (a',an) -> Just (a',an <> b)
    Nothing -> stepRea b t

instance Monoid (Rea t a) where
  mempty = dead
  mappend = (<>)

-- |'still' produces a value that doesn’t react to time and remains still
-- forever.
--
-- Synonym of 'pure'.
still :: a -> Rea t a
still = arr . const

-- |'dead' is a reactive value that doesn’t react to time and doesn’t carry
-- any value. It’s then /inhibiting/.
--
-- If you want that reactive value to produce again, you have to 'revive' it.
-- See 'revive' or '(~>)' for further details.
dead :: Rea t a
dead = Rea (const Nothing)

-- |'one a' pulses the value 'a' and inhibit forever.
one :: a -> Rea t a
one a = pure a <> dead

-- |The identity reactive value. Produces the current time.
now :: Rea t t
now = Rea $ return . (,now)

-- |Produces the 'a' value for the given period of time. Afterward, it
-- inhibits forever.
for :: (Num t,Ord t) => a -> t -> Rea t a
for a duration = Rea $ \t -> do
    (start,_) <- stepRea now t
    stepRea (Rea $ forFrom start) t
  where
    forFrom start t
      | t - start <= duration = Just (a,Rea $ forFrom start)
      | otherwise = Nothing

-- |'Event t a' is a stream of events occurring at 't' times and carrying 'a'
-- values.
newtype Event t a = Event { unEvent :: Rea t a } deriving (Applicative,Functor,Semigroup,Monoid)

-- |An event that won’t ever occur.
never :: Event t a
never = mempty

-- |@always a@ will always occur with 'a' carried.
always :: a -> Event t a
always = pure

-- |Produce an 'Event' that happens only once.
once :: a -> Event t a
once = Event . one

-- |Forget the first few events.
dropE :: Int -> Event t a -> Event t a
dropE 0 e = e
dropE n e = Event . Rea $ \t -> do
  (_,en) <- stepRea (unEvent e) t
  stepRea (unEvent . dropE (pred n) $ Event en) t

-- |Forget the first event.
initE :: Event t a -> Event t a
initE = dropE 1

-- |Check whether an 'Event' has happened yet. If so, @occurred e t@ gives
-- the carried value of the 'Event' along with the next 'Event' in the
-- stream. Otherwise, if no event has occurred, it produces 'Nothing'.
occurred :: Event t a -> t -> Maybe (a,Event t a)
occurred e t = Event <$$> stepRea (unEvent e) t

-- |Build an 'Event' that carries the given value /at/ the given time.
at :: (Ord t) => a -> t -> Event t a
at a t = Event (Rea go)
  where
    go t'
      | t' >= t = Just (a,unEvent never)
      | otherwise = Nothing

-- |Reactive value switch. 'till' takes the initial reactive value and
-- produces it until the 'Event' occurs, afterwards the reactive value
-- is switched with the one carried by the 'Event'.
till :: Rea t a -> Event t (Rea t a) -> Rea t a
till ini e = Rea go
  where
    go t = case occurred e t of
      Just (next,_) -> stepRea next t
      Nothing -> do
        (x,_) <- stepRea ini t
        return (x,till ini e)

-- |Revive a reactive value. @a ~> b@ will produce 'a' until it starts
-- /inhibiting/, afterwhile 'b' is used.
(~>) :: Rea t a -> Rea t a -> Rea t a
a ~> b = Rea $ \t -> case stepRea a t of
  Just (x,a') -> Just (x,a' ~> b)
  Nothing -> stepRea b t

-- |Prefix version of '(~>)'.
revive :: Rea t a -> Rea t a -> Rea t a
revive = (~>)

-- |@a >~ b@ produces 'a' until 'b' doesn’t inhibit anymore.
(>~) :: Rea t a -> Rea t a -> Rea t a
a >~ b = Rea $ \t -> case stepRea b t of
  Just (x,b') -> Just (x,b')
  Nothing -> do
    (x,a') <- stepRea a t
    return (x,a' >~ b)

-- fmap one level deeper!
(<$$>) :: (Functor f,Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
