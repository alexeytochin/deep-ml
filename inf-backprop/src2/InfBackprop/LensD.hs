{-# LANGUAGE TypeOperators #-}

module InfBackprop.LensD where

import Data.Profunctor (Profunctor, dimap, Strong, first', second', Costrong, unfirst, unsecond)
import NumHask ((*), (+), sin, cos, Additive, zero, Multiplicative, one, ExpField, exp, log, (**), sqrt, sinh, cosh, 
  Subtractive, negate, (-),
  Divisive, recip, (/), Field, AdditiveAction, (^), (^^), TrigField, two, Distributive, Integral)
import Prelude (fst, (.), ($), snd, const, undefined, curry, id)
import Data.Bifunctor (bimap)

import InfBackprop.Tangent (Tangent, T)
import Data.Basis3 (Basis, initBackProp , zeroBackProp, End)

--data D dx x dy y  = D {
--  view    :: x -> y,
--  update  :: x -> dy -> dx
--}

newtype LensD dx x dy y = LensD {unpackLensD :: x -> (y, dy -> dx)}

type instance Tangent (LensD dt t dy y) = LensD dt t (T dy) (T y)

view :: LensD dx x dy y -> x -> y
view (LensD x) = fst . x

update :: LensD dx x dy y -> x -> dy -> dx
update (LensD x) = snd . x

(%) :: LensD dy y dz z -> LensD dx x dy y -> LensD dx x dz z
(LensD a2) % (LensD a1) = LensD $ \x -> let
    (y, dyx) = a1 x
    (z, dzy) = a2 y
    dzx = dyx . dzy
  in (z, dzx)

instance Profunctor (LensD dx x) where
  dimap :: (dz -> dy) -> (y -> z) -> LensD dx x dy y -> LensD dx x dz z
  -- dimap f g (D v u) = D (g . v) (\x dz -> u x (f dz))
  dimap f g (LensD a) = LensD $ \x -> let
      (y, dyx) = a x
      y' = g y
      dyx' = dyx . f
    in (y', dyx')

instance Costrong (LensD dt t) where
  -- unfirst  :: p (a, d) (b, d) -> p a b
  unfirst :: LensD dt t (dx, s) (x, s) -> LensD dt t dx x
  -- unfirst (D v u) = D (fst . v) (\t dx -> u t (dx, snd (v t)))
  unfirst (LensD a) = LensD $ \x -> let
      ((y, s), dyx) = a x
      dyx' dx = dyx (dx, s)
    in (y, dyx')

-- tuple
tupleToLens :: Additive dt =>
  (LensD dt t dx1 x1, LensD dt t dx2 x2) -> LensD dt t (dx1, dx2) (x1, x2)
-- tupleToLens (D v1 u1, D v2 u2) = D (\t -> (v1 t, v2 t)) (\t (dy1, dy2) -> u1 t dy1 + u2 t dy2)
tupleToLens (LensD a1, LensD a2) = LensD $ \t -> let
    (x1, dxt1) = a1 t
    (x2, dxt2) = a2 t
  in ((x1, x2), \(dx1, dx2) -> dxt1 dx1 + dxt2 dx2)
-- crossC3 (D v1 u1) (D v2 u2) = D (fork v1 v2) ()

tupleToLens_ :: Additive (T t) =>
  (DFunc t x1, DFunc t x2) -> DFunc t (x1, x2)
tupleToLens_ = tupleToLens

tupleValue :: Additive (T t) =>
  (x -> (DFunc t x1, DFunc t x2)) -> x -> DFunc t (x1, x2)
tupleValue f = tupleToLens_ . f

lensToTuple :: (Additive dx1, Additive dx2) =>
  LensD dt t (dx1, dx2) (x1, x2) -> (LensD dt t dx1 x1, LensD dt t dx2 x2)
-- lensToTuple (D v u) = (D (fst . v) (\t dy1 -> u t (dy1, zero)), D (snd . v) (\t dy2 -> u t (zero, dy2)))
lensToTuple (LensD a) = (LensD a1, LensD a2) where
  a1 = \t -> let
      ((x1, _), dxt) = a t
    in (x1, \dx1 -> dxt (dx1, zero))
  a2 = \t -> let
      ((_, x2), dxt) = a t
    in (x2, \dx2 -> dxt (zero, dx2))


constLensD :: (Additive dx) =>
  y -> LensD dx x dy y
constLensD c = LensD (const (c, const zero))


instance (Additive y, Additive dx) =>
  Additive (LensD dx x dy y) where
    zero = LensD $ const (zero, zero)
    (LensD a1) + (LensD a2) = LensD $ \x -> let
        (y1, dyx1) = a1 x
        (y2, dyx2) = a2 x
      in (y1 + y2, dyx1 + dyx2)

instance (Subtractive y, Subtractive dx) =>
  Subtractive (LensD dx x dy y) where
    negate (LensD a) = LensD (bimap negate negate . a)
    LensD a1 - LensD a2 = LensD $ \x -> let
        (y1, dyx1) = a1 x
        (y2, dyx2) = a2 x
      in (y1 - y2, dyx1 - dyx2)

-- Mult
productLens :: (Multiplicative x, x ~ dx) =>
  LensD dt t (dx, dx) (x, x) -> LensD dt t dx x
productLens (LensD a) = LensD $ \t -> let
    ((x1, x2), dyx) = a t
  in (x1 * x2, \dy -> dyx (dy * x2, x1 * dy))

instance (Additive dt, Multiplicative y, y ~ dy) => -- instance (Additive dx, ) => --
  Multiplicative (LensD dt t dy y) where
    one = constLensD one
    (*) = curry $ productLens . tupleToLens
--    LensD a1 * LensD a2 = LensD $ \x -> let
--        (y1, dyx1) = a1 x
--        (y2, dyx2) = a2 x
--      in (y1 * y2, \dy -> y2 * dyx1 dy + y1 * dyx2 dy)

instance (Additive dx, Distributive dy, y ~ dy) =>
  Distributive (LensD dx x dy y)

--squareC :: (Additive x, Multiplicative x, T x ~ x) => -- , Diff x ~ x) =>
--  DFunc x x
--squareC = LensD $ \x -> (x * x, \dy -> two * x * dy)
--
----square :: (Additive x, Multiplicative x, T x ~ x) =>
----  DP x x x x
----square = lensCtoP squareC
--
--recipC :: (Divisive x, Subtractive x, T x ~ x) =>
--  LensD x x x x
--recipC = LensD $ \x -> (recip x, \dy -> negate $ recip x^2 * dy)
--
--instance (Divisive x, Subtractive dx, dx ~ x, T x ~ dx) =>
--  Divisive (LensD dx x dx x) where
--    recip = lensCtoP recipC
--
--instance (Field x) => 
--  Field (LensD x x x x)



-- DFunc
type DFunc x y = LensD (T x) x (T y) y

constC :: (Additive (T x)) =>
  y -> DFunc x y
constC c = LensD (const (c, const zero))

constC_ :: (Additive dx) =>
  y -> LensD dx x dy y
constC_ c = LensD (const (c, const zero))


class ToConst a b where
  stopDiff :: a -> b

instance ToConst a a where
  stopDiff = id

instance (Additive dt) =>
  ToConst a (LensD dt t da a) where
    stopDiff = constC_


instance (Basis a, Additive dt) =>
  Basis (LensD dt t da a) where
    type End (LensD dt t da a) b = End a b -- (LensD dt t (T b) b)
    --    basis   :: forall b. (a -> b) -> End a b
    initBackProp :: forall b. (LensD dt t da a -> b) -> End a b
    initBackProp bp = initBackProp (bp . constC_)
    zeroBackProp :: LensD dt t da a
    zeroBackProp = constC_ zeroBackProp