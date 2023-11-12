{-# LANGUAGE TypeOperators #-}

module InfBackprop.LensD where

import Data.Profunctor (Profunctor, dimap, Strong, first', second', Costrong, unfirst, unsecond)
import NumHask ((*), (+), sin, cos, Additive, zero, Multiplicative, one, ExpField, exp, log, (**), sqrt, sinh, cosh, 
  Subtractive, negate, (-),
  Divisive, recip, (/), Field, AdditiveAction, (^), (^^), TrigField, two, Distributive, Integral)
import Prelude (fst, (.), ($), snd, const, undefined)
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

instance (Additive dx, Multiplicative y, y ~ dx) =>
  Multiplicative (LensD dx x dy y) where
    one = constLensD one
    LensD a1 * LensD a2 = LensD $ \x -> let
        (y1, dyx1) = a1 x
        (y2, dyx2) = a2 x
      in (y1 * y2, \dy -> y2 * dyx1 dy + y1 * dyx2 dy)


-- DFunc
type DFunc x y = LensD (T x) x (T y) y

constC :: (Additive (T x)) =>
  y -> DFunc x y
constC c = LensD (const (c, const zero))

constC_ :: (Additive dx) =>
  y -> LensD dx x dy y
constC_ c = LensD (const (c, const zero))


instance (Basis a, Additive dt) =>
  Basis (LensD dt t da a) where
    type End (LensD dt t da a) b = End a b -- (LensD dt t (T b) b)
    --    basis   :: forall b. (a, T a -> b) -> End a b
    --    basis   :: forall b. (T a, T T a -> b) -> End (T a) b
    initBackProp :: forall b. (LensD dt t da a -> b) -> End a b
    initBackProp bp = basis ( bpLens (constC_ ()) where
       -- . (view a)
    --    mkZero  :: a -> T a
    zeroBackProp :: LensD dt t da a -> LensD dt t (T da) (T a)
    zeroBackProp (LensD lf) = LensD $ \t -> let
        (y, _) = lf t 
      in (mkZero y, const zero)