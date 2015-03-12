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
import Data.Function ( fix )
import Data.Monoid ( Monoid(..) )
import Data.Profunctor ( Profunctor(..) )
import Data.Semigroup ( Semigroup(..) )
import Prelude hiding ( (.), id )

-- |@Rea t a@ is a /reactive 'a' value that may react to time 't'.
newtype Rea t a b = Rea { stepRea :: t -> a -> Maybe (b,Rea t a b) } deriving (Functor)

instance Category (Rea t) where
  id = arr id
  x . y = Rea $ \t a -> do
    (yr,yn) <- stepRea y t a
    (xr,xn) <- stepRea x t yr
    return (xr,xn . yn)

instance Arrow (Rea t) where
  arr f = fix $ \r -> Rea $ \_ a -> return (f a,r)
  first f = fix $ \r -> Rea $ \t (b,c) -> do
    (fr,fn) <- stepRea f t b
    return ((fr,c),r)

instance Profunctor (Rea t) where
  dimap f g r = Rea $ \t a -> do
    (rr,rn) <- stepRea r t (f a)
    return (g rr,dimap f g rn)

instance Applicative (Rea t a) where
  pure = still
  f <*> x = Rea $ \t a -> do
    (fr,fn) <- stepRea f t a
    (xr,xn) <- stepRea x t a
    return $ (fr xr,fn <*> xn)

instance (Semigroup b) => Semigroup (Rea t a b) where
  x <> y = Rea $ \t a -> do
    (xr,xn) <- stepRea x t a
    (yr,yn) <- stepRea y t a
    return $ (xr <> yr,xn <> yn)

instance (Semigroup b) => Monoid (Rea t a b) where
  mempty = dead
  mappend = (<>)

-- |'still' produces a value that doesn’t react to time and remains still
-- forever.
--
-- Synonym of 'pure'.
still :: b -> Rea t a b
still = arr . const

-- |'dead' is a reactive value that doesn’t react to time and doesn’t carry
-- any value. It’s then /inhibiting/.
--
-- If you want that reactive value to produce again, you have to 'revive' it.
-- See 'revive' or '(~>)' for further details.
dead :: Rea t a b
dead = Rea $ \_ _ -> Nothing

-- |'one a' pulses the value 'a' and inhibit forever.
one :: b -> Rea t a b
one b = Rea $ \_ _ -> Just (b,dead)

-- |Produces the current time.
now :: Rea t a t
now = Rea $ \t _ -> return (t,now)

-- |Produces the 'a' value for the given period of time. Afterward, it
-- inhibits forever.
{-
for :: (Num t,Ord t) => b -> t -> Rea t a b
for x duration = Rea $ \t a -> do
    (start,_) <- stepRea now t a
    stepRea (Rea $ forFrom start) t
  where
    forFrom start t
      | t - start <= duration = Just (a,Rea $ forFrom start)
      | otherwise = Nothing
-}

-- |'Event t a' is a stream of events occurring at 't' times and carrying 'a'
-- values.
newtype Event t a = Event { unEvent :: Rea t () a } deriving (Applicative,Functor,Semigroup,Monoid)

-- |An event that won’t ever occur.
never :: Event t a
never = Event . Rea $ \_ _ -> Nothing

-- |@always a@ will always occur with 'a' carried.
always :: a -> Event t a
always = pure

-- |Produce an 'Event' that happens only once.
once :: a -> Event t a
once = Event . one

-- |Forget the first few events.
dropE :: Int -> Event t a -> Event t a
dropE 0 e = e
dropE n e = Event . Rea $ \t _ -> do
  (_,en) <- stepRea (unEvent e) t ()
  stepRea (unEvent . dropE (pred n) $ Event en) t ()

-- |Forget the first event.
initE :: Event t a -> Event t a
initE = dropE 1

-- |Check whether an 'Event' has happened yet. If so, @occurred e t@ gives
-- the carried value of the 'Event' along with the next 'Event' in the
-- stream. Otherwise, if no event has occurred, it produces 'Nothing'.
occurred :: Event t a -> t -> Maybe (a,Event t a)
occurred e t = Event <$$> stepRea (unEvent e) t ()

-- |Build an 'Event' that carries the given value /at/ the given time.
at :: (Ord t) => a -> t -> Event t a
at v t = Event $ Rea go
  where
    go t' _
      | t' >= t = return (v,dead)
      | otherwise = Nothing

-- |Reactive value switch. 'till' takes the initial reactive value and
-- produces it until the 'Event' occurs, afterwards the reactive value
-- is switched with the one carried by the 'Event'.
till :: Rea t a b -> Event t (Rea t a b) -> Rea t a b
till ini e = fix $ \r -> Rea $ \t a -> case occurred e t of
  Just (next,_) -> stepRea next t a
  Nothing -> do
    (x,_) <- stepRea ini t a
    return (x,r)

-- |Revive a reactive value. @a ~> b@ will produce 'a' until it starts
-- /inhibiting/, afterwhile 'b' is used.
(~>) :: Rea t a b -> Rea t a b -> Rea t a b
x ~> y = Rea $ \t a -> case stepRea x t a of
  Just (r,xn) -> Just (r,xn ~> y)
  Nothing -> stepRea y t a

-- |Prefix version of '(~>)'.
revive :: Rea t a b -> Rea t a b -> Rea t a b
revive = (~>)

-- |@a >~ b@ produces 'a' until 'b' doesn’t inhibit anymore.
(>~) :: Rea t a b -> Rea t a b -> Rea t a b
x >~ y = Rea $ \t a -> case stepRea y t a of
  Just (r,yn) -> Just (r,yn)
  Nothing -> do
    (r,xn) <- stepRea x t a
    return (r,xn >~ y)

-- fmap one level deeper!
(<$$>) :: (Functor f,Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
