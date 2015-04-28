module FRP.Reactant (
    -- * Automatons
    Auto(..)
  , liftTA
  , liftT
  , liftFixTA
  , liftFixT
  , time
  , filterE
  , mute
  , for
  , after
  , at
  , when
  , unless
  , (~>)
  , (>~)
  , until 
    -- * Testing
  , stdinTest
    -- * Re-exported modules
  , module Control.Arrow
  , module Control.Category
  , module Data.Profunctor
  , module Data.Semigroup
  ) where

import Control.Applicative ( Applicative(..), liftA2 )
import Control.Arrow ( Arrow(..) )
import Control.Category ( Category(..) )
import Control.Monad ( guard )
import qualified Control.Monad as M ( unless )
import Data.Function ( fix )
import Data.Profunctor ( Profunctor(..) )
import Data.Monoid ( Monoid(..) )
import Data.Semigroup ( Semigroup(..) )
import Data.String ( IsString(..) )
import Prelude hiding ( (.), id, until )

-- |@Auto t a m b@ is a signal automaton from 'a' to 'b' consuming time 't'.
newtype Auto t a b = Auto { runAuto :: t -> a -> (Maybe b,Auto t a b) }

instance Applicative (Auto t a) where
  pure b = arr (const b)
  Auto f <*> Auto x = Auto $ \t a ->
    let
      (f',nf) = f t a
      (x',nx) = x t a
    in (f' <*> x',nf <*> nx)

instance Arrow (Auto t) where
  arr f = fix $ \r -> Auto $ \t a -> (Just $ f a,r)
  first (Auto f) = Auto $ \t (a,b) -> let (f',nf) = f t a in (fmap (,b) f',first nf)
  second (Auto f) = Auto $ \t (a,b) -> let (f',nf) = f t b in (fmap (a,) f',second nf)

instance Category (Auto t) where
  id = arr id
  x@(Auto g) . Auto f = Auto $ \t a ->
    let (f',nf) = f t a in case f' of
      Just f'' -> fmap (. nf) $ g t f''
      Nothing -> (Nothing,x . nf)

instance Functor (Auto t a) where
  fmap f (Auto x) = Auto $ \t a -> let (xr,nx) = x t a in (fmap f xr,fmap f nx)

instance (IsString b) => IsString (Auto t a b) where
  fromString = pure . fromString

-- TODO: remove the Semigroup constraint when possible
instance (Monoid b,Semigroup b) => Monoid (Auto t a b) where
  mempty = mute
  mappend = (<>)

instance (Num b) => Num (Auto t a b) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Profunctor (Auto t) where
  dimap l r (Auto x) = Auto $ \t a -> let (xr,nx) = x t (l a) in (fmap r xr,dimap l r nx)

instance (Semigroup b) => Semigroup (Auto t a b) where
  Auto x <> Auto y = Auto $ \t a ->
    let
      (x',nx) = x t a
      (y',ny) = y t a
    in (x' <> y',nx <> ny)

-- |Lift a time-aware pure function into @Auto t a b@.
liftTA :: (t -> a -> b) -> Auto t a b
liftTA f = liftFixTA $ \t a -> Just $ f t a

-- |Lift a time function into @Auto t a b@. Discard 'a'.
liftT :: (t -> b) -> Auto t a b
liftT f = liftFixT $ Just . f

-- |Lift a fixed time-aware pure function into @Auto t a b@.
liftFixTA :: (t -> a -> Maybe b) -> Auto t a b
liftFixTA f = fix $ \r -> Auto $ \t a -> (f t a,r)

-- |Lift a fixed time pure function into @Auto t a b@.
liftFixT :: (t -> Maybe b) -> Auto t a b
liftFixT f = fix $ \r -> Auto $ \t _ -> (f t,r)

-- |Monotonic time, which is 0 when the 'Auto' starts to produce and goes on
-- infinitely. If you need the absolute time of the simulation, consider
-- using 'atime'.
time :: (Num t) => Auto t a t
time = Auto $ \t a -> runAuto (liftT $ \t' -> t' - t) t a

-- |Absolute time.
atime :: Auto t a t
atime = liftT id

filterE :: (b -> Bool) -> Auto t (Maybe b) (Maybe b)
filterE p = arr $ \m -> do
  b <- m
  guard (p b)
  pure b

-- |Inhibit forever.
mute :: Auto t a b
mute = liftFixT $ const Nothing

-- |Produce for a given period of time then inhibit forever.
for :: (Num t,Ord t) => t -> Auto t a a
for dur = Auto $ \t a -> runAuto (go t) t a
  where
    go t = liftFixTA $ \t' a -> if t' - t < dur then Just a else Nothing

-- |Inhibit for a given period of time, then produce for ever.
after :: (Num t,Ord t) => t -> Auto t a a
after dur = Auto $ \t a -> runAuto (go t) t a
  where
    go t = liftFixTA $ \t' a -> if t' - t <= dur then Nothing else Just a

-- |Produce only at the given time.
--
-- You may not want to use a simple floating type with 'at' because of
-- rounding error. Consider using a wrapper that implements 'Eq' with
-- epsilon-biased comparison.
at :: (Eq t) => t -> Auto t a a
at atT = liftFixTA $ \t a -> if t == atT then Just a else Nothing

-- |@when p@ behave as 'id' when the 'p' predicate is true. inhibits
-- otherwise.
when :: (a -> Bool) -> Auto t a a
when p = liftFixTA $ \_ a -> guard (p a) >> pure a

-- |@unless p@ behave as 'id' unless the 'p' predicate is true. Inhibits
-- otherwise.
unless :: (a -> Bool) -> Auto t a a
unless = when . (not .)

-- |@x ~> y@ produces with 'y' until it stops producing, then it uses 'y'
-- forever.
infixr 6 ~>
(~>) :: Auto t a b -> Auto t a b -> Auto t a b
Auto x ~> y =
  Auto $ \t a -> let (x',nx) = x t a in maybe (runAuto y t a) (const $ (x',nx ~> y)) x'

-- |@x >~ y@ produces with 'x' until 'y' starts producing, no matter whether 'x'
-- inhibits.
(>~) :: Auto t a b -> Auto t a b -> Auto t a b
Auto x >~ Auto y =
  Auto $ \t a ->
    let
      (x',nx) = x t a
      (y',ny) = y t a
    in case y' of
      Just _ -> (y',ny)
      Nothing -> (x',nx ~> ny)

-- |'until' produces with 'a' until its event @Maybe b@ occurs. When so,
-- inhibits forever.
until :: Auto t (a,Maybe b) a
until = fix $ \r -> Auto $ \t (a,b) ->
  maybe (Just a,r) (const $ runAuto mute t (a,b)) b

-- |Test a @(Read a,Show b,Num t) => Auto t a b@ by providing input via
-- /stdin/ and outputing results on stdout.
stdinTest :: (Read a,Show b,Num t) => (t -> t) -> Auto t a b -> IO ()
stdinTest incr initial = go initial 0
  where
    go x t = do
      rawInput <- fmap reads getLine
      M.unless (null rawInput) $ do
        let
          [(input,_)] = rawInput
          (x',nx) = runAuto x t input
        print x'
        go nx (incr t)
