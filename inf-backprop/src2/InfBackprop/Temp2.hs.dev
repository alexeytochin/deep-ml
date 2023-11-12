{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module InfBackprop.Temp2 where

--import NumHask (sinh, TrigField, Field, Ring, Additive, Multiplicative, (+), (*), Subtractive, Distributive, Divisive)
--import Prelude ((*), undefined)
--import qualified NumHask as NH
--import qualified Prelude as P

import Prelude ((.), id, fst, snd, uncurry, curry, ($), undefined, const, fmap, Functor)
import qualified Prelude as P
import NumHask ((*), (+), sin, cos, Additive, zero, Multiplicative, one, ExpField, exp, log, (**), sqrt, sinh, cosh, 
  Subtractive, negate, (-),
  Divisive, recip, (/), Field, AdditiveAction, (^), (^^), TrigField, two, Distributive, Integral, Ring)
import qualified NumHask as NH (sqrt, sinh, cosh)
import Data.Bifunctor (bimap)
import GHC.Base (Type)
import NumHask.Prelude (Float, flip)
--import Prelude hiding (sinh, cosh, (*), (+), (-), negate, recip, exp, (^), (^^), (/), log)
import Data.Void (Void, absurd)
import Data.Profunctor.Extra (Outer, (***), outer)
import Data.Profunctor (Profunctor, dimap, Strong, first', second', Costrong, unfirst, unsecond)
import Debug.SimpleExpr.Expr (number)
import Data.Linear (unitDual, tupleDual, listDual)


class MultiplicativeAction a b c | a b -> c where
  infixl 7 ..*
  (..*) :: a -> b -> c

instance MultiplicativeAction Float Float Float where
  (..*) = (*)
  
instance (MultiplicativeAction a b1 c1, MultiplicativeAction a b2 c2) =>
  MultiplicativeAction a (b1, b2) (c1, c2) where
    a ..* (b1, b2) = (a ..* b1, a ..* b2)

instance (MultiplicativeAction a b1 c1, MultiplicativeAction a b2 c2, MultiplicativeAction a b3 c3) =>
  MultiplicativeAction a (b1, b2, b3) (c1, c2, c3) where
    a ..* (b1, b2, b3) = (a ..* b1, a ..* b2, a ..* b3)


cross :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
cross f g (x, y) = (f x, g y)

fork :: (t -> a) -> (t -> b) -> t -> (a, b)
fork f g x = (f x, g x)


--data D dx x dy y  = D {
--  view    :: x -> y,
--  update  :: x -> dy -> dx
--}

newtype D dx x dy y  = D {unpackD :: x -> (y, dy -> dx)}

view :: D dx x dy y -> x -> y
view (D x) = fst . x

update :: D dx x dy y -> x -> dy -> dx
update (D x) = snd . x


(%) :: D dy y dz z -> D dx x dy y -> D dx x dz z
-- (D v2 u2) % (D v1 u1) = D (v2 . v1) (\x dz -> u1 x (u2 (v1 x) dz))
(D a2) % (D a1) = D $ \x -> let
    (y, dyx) = a1 x
    (z, dzy) = a2 y
    dzx = dyx . dzy
  in (z, dzx)


instance Profunctor (D dx x) where
  dimap :: (dz -> dy) -> (y -> z) -> D dx x dy y -> D dx x dz z
  -- dimap f g (D v u) = D (g . v) (\x dz -> u x (f dz))
  dimap f g (D a) = D $ \x -> let
      (y, dyx) = a x
      y' = g y
      dyx' = dyx . f
    in (y', dyx')

instance Costrong (D dt t) where
  -- unfirst  :: p (a, d) (b, d) -> p a b
  unfirst :: D dt t (dx, s) (x, s) -> D dt t dx x
  -- unfirst (D v u) = D (fst . v) (\t dx -> u t (dx, snd (v t)))
  unfirst (D a) = D $ \x -> let
      ((y, s), dyx) = a x
      dyx' dx = dyx (dx, s)
    in (y, dyx')



constC :: (Differentiable x dx, Differentiable y dy, Additive dx) =>
  y -> D dx x dy y
constC c = D (const (c, const zero))

class Const c b a where
  const' :: c -> b -> a

instance Const c b c where
  const' = const

instance (Differentiable b db, Differentiable c dc, Additive db) => 
  Const c (D dt t db b) (D dt t dc c) where
    const'= lensCtoP . constC


-- Differentiable
class Differentiable x dx | x -> dx where --  
  startBackprop :: x -> dx

instance Differentiable Float Float where
  startBackprop = const 1

instance (Differentiable a da, Differentiable b db) =>
  Differentiable (a, b) (da, db) where
    startBackprop = bimap startBackprop startBackprop

instance (Differentiable a da, Differentiable b db, Differentiable c dc) =>
  Differentiable (a, b, c) (da, db, dc) where
  startBackprop = \(a, b, c) -> (startBackprop a, startBackprop b, startBackprop c)

--instance (Functor m, Differentiable a da) =>
--  Differentiable (m a) (m da) where
--  startBackprop = fmap startBackprop

instance (Differentiable x dx, Differentiable dx ddx, Differentiable ddx dx) =>
    Differentiable (D dt t dx x) (D dt t ddx dx) where
  startBackprop (D a) = D $ \t -> let
      (x, dxt) = a t
    in (startBackprop x, dxt . startBackprop) 


tupleToLens :: Additive dt =>
  (D dt t dx1 x1, D dt t dx2 x2) -> D dt t (dx1, dx2) (x1, x2)
-- tupleToLens (D v1 u1, D v2 u2) = D (\t -> (v1 t, v2 t)) (\t (dy1, dy2) -> u1 t dy1 + u2 t dy2)
tupleToLens (D a1, D a2) = D $ \t -> let
    (x1, dxt1) = a1 t
    (x2, dxt2) = a2 t
  in ((x1, x2), \(dx1, dx2) -> dxt1 dx1 + dxt2 dx2)
-- crossC3 (D v1 u1) (D v2 u2) = D (fork v1 v2) ()

lensToTuple :: (Additive dx1, Additive dx2) =>
  D dt t (dx1, dx2) (x1, x2) -> (D dt t dx1 x1, D dt t dx2 x2)
-- lensToTuple (D v u) = (D (fst . v) (\t dy1 -> u t (dy1, zero)), D (snd . v) (\t dy2 -> u t (zero, dy2)))
lensToTuple (D a) = (D a1, D a2) where
  a1 = \t -> let
      ((x1, _), dxt) = a t
    in (x1, \dx1 -> dxt (dx1, zero))
  a2 = \t -> let
      ((_, x2), dxt) = a t
    in (x2, \dx2 -> dxt (zero, dx2))

functionToLens :: (r -> D dt t dx x) -> D dt t (dx, r) (r -> x)
functionToLens f = D $ \t ->
  (\r -> view (f r) t, \(dx, r) -> update (f r) t dx)

--lensToTriple :: (Additive dx1, Additive dx2, Additive dx3) =>
--  D dt t (dx1, dx2, dx3) (x1, x2, x3) -> (D dt t dx1 x1, D dt t dx2 x2, D dt t dx3 x3)
--lensToTriple (D a) = (D a1, D a2, D a3) where
--  a1 = \t -> let
--      ((x1, _, _), dxt) = a t
--    in (x1, \dx1 -> dxt (dx1, zero, zero))
--  a2 = \t -> let
--      ((_, x2, _), dxt) = a t
--    in (x2, \dx2 -> dxt (zero, dx2, zero))
--  a3 = \t -> let
--      ((_, _, x3), dxt) = a t
--    in (x3, \dx3 -> dxt (zero, zero, dx3))


-- DP
type DP dx x dy y = forall p. Costrong p => p dx x -> p dy y

identity :: D a b a b
-- identity = D id (const id)
identity = D (, id)

lensCtoP :: D dx x dy y -> DP dx x dy y
-- lensCtoP (D v u) = unsecond . dimap (uncurry u) (fork id v)
lensCtoP l = unsecond . dimap (uncurry u) (fork id v) where
  v = view l
  u = update l

lensPtoC :: DP dx x dy y -> D dx x dy y
lensPtoC f = f identity



--class DiffFormIn a a' | a -> a' where
--  differentiableForm :: f -> (a -> b)
--
--instance DiffFormIn (a -> b) a b where
--  differentiableForm = id
--
--instance DiffFormIn (() -> b) a b where
--  differentiableForm = id


--differentiableForm :: (DiffFormIn a a', DiffFormOut b b') => (a -> b) -> (a' -> b')
--differentiableForm f = diffFormOut . f . diffFormIn 


instance (Additive y, Additive dx) =>
  Additive (D dx x dy y) where
    -- zero = D (const zero) (const zero)
    zero = D $ const (zero, zero)
    -- (D v1 u1) + (D v2 u2) = D (\x -> v1 x + v2 x) (\t -> u1 t + u2 t)
    (D a1) + (D a2) = D $ \x -> let
        (y1, dyx1) = a1 x
        (y2, dyx2) = a2 x
      in (y1 + y2, dyx1 + dyx2)

instance (Subtractive a, Subtractive b) =>
  Subtractive (a, b) where
    negate = cross negate negate
    (a, b) - (c, d) = (a - c, b - d)

instance (Subtractive y, Subtractive dx) =>
  Subtractive (D dx x dy y) where
    -- negate (D v u) = D (negate v) (negate u)
    negate (D a) = D (bimap negate negate . a)
    -- (D v1 u1) - (D v2 u2) = D (\x -> v1 x - v2 x) (\t -> u1 t - u2 t)
    D a1 - D a2 = D $ \x -> let
        (y1, dyx1) = a1 x
        (y2, dyx2) = a2 x
      in (y1 - y2, dyx1 - dyx2)

instance (MultiplicativeAction y dx dx, Additive dx,
  MultiplicativeAction y y y, MultiplicativeAction dx y dx) =>
    MultiplicativeAction (D dx x dy y) (D dx x dy y) (D dx x dy y) where
      -- D v1 u1 ..* D v2 u2 = D (\x -> v1 x * v2 x) (\x dy -> v2 x ..* u1 x dy + v1 x ..* u2 x dy)
      D a1 ..* D a2 = D $ \x -> let
          (y1, dyx1) = a1 x
          (y2, dyx2) = a2 x
        in (y1 ..* y2, \dy -> dyx1 dy ..* y2 + y2 ..* dyx2 dy)

instance (MultiplicativeAction y dx dx, Additive dx, Multiplicative y, Differentiable x dx, Differentiable y dy) =>
  Multiplicative (D dx x dy y) where
--    one = D (const one) (const zero)
    one = constC one
--    (D v1 u1) * (D v2 u2) = D (\x -> v1 x * v2 x) (\x dy -> v2 x ..* u1 x dy + v1 x ..* u2 x dy)
    D a1 * D a2 = D $ \x -> let
        (y1, dyx1) = a1 x
        (y2, dyx2) = a2 x
      in (y1 * y2, \dy -> y2 ..* dyx1 dy + y1 ..* dyx2 dy)

instance (Distributive a, Distributive b) =>
  Distributive (a, b)

instance (Distributive y, Distributive dx, MultiplicativeAction y dx dx, Differentiable x dx, Differentiable y dy) =>
  Distributive (D dx x dy y) where


--derivative_ :: -- Differentiable y dy =>
--  (D dx x dx x -> D dx x dy y) -> x -> dy -> dx
--derivative_ f = update (f identity)

--derivativeOp :: -- Differentiable y dy =>
--  (D dx x dx x -> D dx x dy y) -> dy -> x -> dx
--derivativeOp f = flip $ update (f identity)

derivativeOp :: (D dx x dx x -> D dx x dy y) -> x -> dy -> dx
--derivativeOp :: (D dt t dx x -> D dt t dy y) -> x -> dy -> dx
derivativeOp f = update (f identity)

derivativeValue_ ::
  (D dx x dx x -> D dx x dy y) -> x -> (y, dy -> dx)
derivativeValue_ f = unpackD (f identity)

--type family Diff x :: Type
--type instance Diff Float = Float
--type instance Diff (a, b) = (Diff a, Diff b)
--type instance Diff (a, b, c) = (Diff a, Diff b, Diff c)


--derivative :: (Differentiable y dy) =>
--  (D dx x dx x -> D dx x dy y) -> x -> dy'
--derivative f x = basis $ derivative_ f x 


lensToTriple :: (Additive dx1, Additive dx2, Additive dx3) =>
  D dt t (dx1, dx2, dx3) (x1, x2, x3) -> (D dt t dx1 x1, D dt t dx2 x2, D dt t dx3 x3)
lensToTriple (D a) = (D a1, D a2, D a3) where
  a1 = \t -> let
      ((x1, _, _), dxt) = a t
    in (x1, \dx1 -> dxt (dx1, zero, zero))
  a2 = \t -> let
      ((_, x2, _), dxt) = a t
    in (x2, \dx2 -> dxt (zero, dx2, zero))
  a3 = \t -> let
      ((_, _, x3), dxt) = a t
    in (x3, \dx3 -> dxt (zero, zero, dx3))


class DiffIn a dx x dy y | a -> dx x dy y where
  fromD :: D dx x dy y -> a

instance DiffIn (D dx x dy y) dx x dy y where
  fromD = id

instance (Additive dx1, Additive dx2) =>
  DiffIn (D dt t dx1 x1, D dt t dx2 x2) dt t (dx1, dx2) (x1, x2) where
    fromD = lensToTuple

instance (Additive dt, Additive dx1, Additive dx2, Additive dx3) =>
  DiffIn (D dt t dx1 x1, D dt t dx2 x2, D dt t dx3 x3) dt t (dx1, dx2, dx3) (x1, x2, x3) where
    fromD (D a) = (D a1, D a2, D a3) where
      a1 = \t -> let
          ((x1, _, _), dxt) = a t
        in (x1, \dx1 -> dxt (dx1, zero, zero))
      a2 = \t -> let
          ((_, x2, _), dxt) = a t
        in (x2, \dx2 -> dxt (zero, dx2, zero))
      a3 = \t -> let
          ((_, _, x3), dxt) = a t
        in (x3, \dx3 -> dxt (zero, zero, dx3))


class DiffOut a dx x dy y | a -> dx x dy y where
  toD   :: a -> D dx x dy y

instance DiffOut (D dx x dy y) dx x dy y where
  toD   = id

instance (Additive dt, Additive dx1, Additive dx2) =>
  DiffOut (D dt t dx1 x1, D dt t dx2 x2) dt t (dx1, dx2) (x1, x2) where
    toD   = tupleToLens

instance (Additive dt, Additive dx1, Additive dx2, Additive dx3) =>
  DiffOut (D dt t dx1 x1, D dt t dx2 x2, D dt t dx3 x3) dt t (dx1, dx2, dx3) (x1, x2, x3) where
    toD (D a1, D a2, D a3) = D $ \t -> let
        (x1, dxt1) = a1 t
        (x2, dxt2) = a2 t
        (x3, dxt3) = a3 t
      in ((x1, x2, x3), \(dx1, dx2, dx3) -> dxt1 dx1 + dxt2 dx2 + dxt3 dx3)

diffForm :: (DiffIn a dt t dx x, DiffOut b dt t dy y) =>
  (a -> b) -> D dt t dx x -> D dt t dy y
diffForm f = toD . f . fromD

derivativeOpAuto :: (DiffIn a dx x dx x, DiffOut b dx x dy y, Differentiable y dy) =>
  (a -> b) -> x -> dy -> dx
derivativeOpAuto f = derivativeOp $ diffForm f
-- derivativeOp :: (D dx x dx x -> D dx x dy y) -> x -> dy -> dx

-- Functions
squareC :: (Additive x, MultiplicativeAction x dx dx, Multiplicative x) =>
  D dx x dx x
--squareC = D (\x -> x * x) (\x dy -> two * x ..* dy)
squareC = D $ \x -> (x * x, \dy -> two * x ..* dy)

square :: (Additive x, MultiplicativeAction x dx dx, Multiplicative x) =>
  DP dx x dx x
square = lensCtoP squareC

recipC :: (Divisive x, MultiplicativeAction x dx dx, Subtractive dx) =>
  D dx x dx x
--recipC = D recip (\x dy -> negate $ recip x^2 ..* dy)
recipC = D $ \x -> (recip x, \dy -> negate $ recip x^2 ..* dy)

instance (Divisive a, Divisive b) =>
  Divisive (a, b) where
    recip = cross recip recip

instance (Divisive x, MultiplicativeAction x dt dt, MultiplicativeAction x dx dx, Subtractive dx, Additive dt,
  Differentiable t dt, Differentiable x dx) =>
    Divisive (D dt t dx x) where
      recip = lensCtoP recipC

instance (Field a, Field b) =>
  Field (a, b)

instance (Divisive x, MultiplicativeAction x dx dx, MultiplicativeAction x dt dt, Distributive x, Subtractive dx, 
  Subtractive x, Distributive dt, Subtractive dt, Differentiable t dt, Differentiable x dx) =>
    Field (D dt t dx x) where

expC :: (ExpField x, MultiplicativeAction x dx dx) =>
  D dx x dx x
--expC = D exp (\x dy -> exp x ..* dy)
expC = D $ \x -> let 
    expX = exp x 
  in (expX, (expX ..*))

logC :: (ExpField x, MultiplicativeAction x dx dx) =>
  D dx x dx x
logC = D $ \x -> (log x, \dy -> recip x ..* dy)

instance (ExpField x, MultiplicativeAction x dx dx, Distributive dx, Subtractive dx, Differentiable x dx) =>
  ExpField (D dx x dx x) where
    exp = lensCtoP expC
    log = lensCtoP logC
    (**) = undefined -- :: a -> a -> a

sinhC :: (TrigField x, MultiplicativeAction x dx dx) => D dx x dx x
--sinhC = D sinh (\x dy -> cosh x ..* dy)
sinhC = D $ \x -> (sinh x, \dy -> cosh x ..* dy)
coshC :: (TrigField x, MultiplicativeAction x dx dx) => D dx x dx x
--coshC = D cosh (\x dy -> sinh x ..* dy)
coshC = D $ \x -> (cosh x, \dy -> sinh x ..* dy)

instance (ExpField x, MultiplicativeAction x dx dx, Distributive dx, Subtractive dx, Differentiable x dx) => 
  TrigField (D dx x dx x) where
    sinh = undefined

instance TrigField () where
  sinh = undefined

f :: TrigField a => a -> a
f = sinh



instance Multiplicative () where
instance Additive () where
instance Subtractive () where
instance Distributive () where
instance Ring () where
instance Divisive () where
instance Field () where
