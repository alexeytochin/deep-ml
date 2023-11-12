module Numeric.InfBackprop.DFunc2 where

import Prelude ((.), id, curry, const)
import GHC.Base (Type, Float, undefined, ($))
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Fixed.Cont as DVFC
import qualified Data.Vector.Fixed.Boxed as DVFB
import qualified Data.Vector.Fixed as DVF
import qualified Data.Vector as DV
import NumHask (Additive, (+), zero, Subtractive,
  TrigField, sin, cos, (-), negate, asin, acos, atan, atan2, sinh, cosh, asinh, acosh, atanh, pi, Multiplicative, (*))
import NumHask.AdvancedActions ((.*), (*.), LeftMultiplicativeAction, RightMultiplicativeAction, LeftRightMultiplicativeAction)
import Control.Category (Category)
import Prelude.Tools (cross)
  
  
type ContangentVectorField a = ContangentVectorField2 a a
type CVF a = ContangentVectorField a


type family ContangentVectorField2 (a :: Type) (b :: Type) :: Type
type instance ContangentVectorField2 a Float = a -> a
type instance ContangentVectorField2 a (DVFB.Vec n b) = ContangentVectorField2 a b


data DFunc a b c d = MkDFunc {
  call :: a -> c,
  pullback :: d -> b
}

--run :: DFunc a b c (c -> d) -> (c -> d) -> (c, b)
--run (MkDFunc _ pb) initBackprop = (\y -> (y, pb b)) (\y -> (y, initBackprop y))
  



identity :: DFunc a b a b
identity = MkDFunc id id

(%) :: DFunc a b c d -> DFunc e f a b -> DFunc e f c d
(MkDFunc c2 p2) % (MkDFunc c1 p1) = MkDFunc c p where
  c = c2 . c1
  p = p1 . p2

tupleToDFunc :: Additive b =>
  (DFunc a b c0 d0, DFunc a b c1 d1) -> DFunc a b (c0, c1) (d0, d1)
tupleToDFunc (MkDFunc call0 pullback0, MkDFunc call1 pullback1) = MkDFunc c p where
  c x = (call0 x, call1 x)
  p (d0, d1) = pullback0 d0 + pullback1 d1

--tupleToDFunc2 :: Additive b =>
--  (DFunc a b c0 (c0 -> d0), DFunc a b c1 (c1 -> d1)) -> DFunc a b (c0, c1) ((c0, c1) -> (d0, d1))
--tupleToDFunc2 (MkDFunc call0 pullback0, MkDFunc call1 pullback1) = MkDFunc c p where
--  c x = (call0 x, call1 x)
--  p my y = pullback0 y0 + pullback1 y1 where
--    (cy0, cy1) = my

--    (x1, dxt1) = a1 t
--    (x2, dxt2) = a2 t
--  in ((x1, x2), \(dx1, dx2) -> dxt1 dx1 + dxt2 dx2)


dFuncToUnnaryFunc :: DFunc a b c d -> DFunc e f a b -> DFunc e f c d
dFuncToUnnaryFunc = (%)

--(%) :: LensD dy y dz z -> LensD dx x dy y -> LensD dx x dz z
--(LensD a2) % (LensD a1) = LensD $ \x -> let
--    (y, dyx) = a1 x
--    (z, dzy) = a2 y
--    dzx = dyx . dzy
--  in (z, dzx)

-- Sum
sumD :: (Additive a) =>
  DFunc (a, a) (b, b) a b
sumD = MkDFunc c p where
  c (x0, x1) = x0 + x1
  p my = (my, my)

instance (Additive mt, Additive a) =>
  Additive (DFunc t mt a b) where
    zero = MkDFunc (const zero) (const zero)
    (+) = curry $ dFuncToUnnaryFunc sumD . tupleToDFunc

subD :: (Subtractive a, Subtractive b) =>
  DFunc (a, a) (b, b) a b
subD = MkDFunc c p where
  c (x0, x1) = x0 - x1
  p my = (my, negate my)

instance (Subtractive mt, Subtractive a, Subtractive b) =>
  Subtractive (DFunc t mt a b) where
    (-) = curry $ dFuncToUnnaryFunc subD . tupleToDFunc

--multD :: (Multiplicative a, LeftRightMultiplicativeAction a b) =>
----  DFunc (a, a) ((a, a) -> (b, b)) a (a -> b)
--  DFunc (a, a) ((a, a) -> b, (a, a) -> b) a (a -> b)
--multD = MkDFunc c p where
--  c (x0, x1) = x0 * x1
--  p my = (\x -> x .* my x, \x -> my x *. x)
--
--multD3 :: (Multiplicative a, LeftRightMultiplicativeAction a b) =>
--  DFunc (a, a, a) ((a, a, a) -> (b, b, b)) a (a -> b)
--multD3 = MkDFunc c p where
--  c (x0, x1, x2) = x0 * x1 * x2
--  p my (x0, x1, x2) = (x1 .* x2 .* cy, x0 .* x2 .* cy, x0 .* x1 .* cy) where
--    cy = my $ c (x0, x1, x2)

--multDFunc :: (Multiplicative a, LeftRightMultiplicativeAction a b) =>
--  DFunc t mt a (a -> b) -> DFunc t mt a (a -> b) -> DFunc t mt a (a -> b)
--multDFunc (MkDFunc call0 pullback0) (MkDFunc call1 pullback1) = MkDFunc c p where
--  c t = call0 t * call1 t
--  p my t = (x1 .* x2 .* cy, x0 .* x2 .* cy, x0 .* x1 .* cy) where
--    cy = my $ c (x0, x1, x2)

--multD :: (Multiplicative a, LeftRightMultiplicativeAction a b) =>
----  DFunc (a, a) ((a, a) -> (b, b)) a (a -> b)
--  DFunc (a, a) (b, b) a b
--multD = MkDFunc c p where
--  c (x0, x1) = x0 * x1
--  p my = (x .* my, my *. x)

--instance (Subtractive mt, Multiplicative a, Subtractive b) =>
--  Multiplicative (DFunc t mt a b) where
--    (*) = curry $ dFuncToUnnaryFunc multD . tupleToDFunc






sinDFunc :: (TrigField a, LeftMultiplicativeAction a b) =>
  DFunc a (a -> b) a (a -> b)
sinDFunc = MkDFunc c p where
  c = sin
  p ctm x = cos x .* ctm x

cosDFunc :: (TrigField a, LeftMultiplicativeAction a b) =>
  DFunc a (a -> b) a (a -> b)
cosDFunc = MkDFunc c p where
  c = cos
  p ctm x = negate sin x .* ctm x

--instance (Subtractive b, TrigField a, LeftMultiplicativeAction a b, Subtractive mt) =>
--  TrigField (DFunc t mt a (a -> b)) where
--    sin = dFuncToUnnaryFunc sinDFunc
--    cos = dFuncToUnnaryFunc cosDFunc
--    asin = undefined -- lensToUnnaryFunc asinC
--    acos = undefined -- lensToUnnaryFunc acosC
--    atan = undefined -- lensToUnnaryFunc atanC
--    atan2 = undefined -- lensToBinaryFunc atan2C
--    sinh = undefined -- lensToUnnaryFunc sinhC
--    cosh = undefined -- lensToUnnaryFunc coshC
--    asinh = undefined -- lensToUnnaryFunc asinhC
--    acosh = undefined -- lensToUnnaryFunc acoshC
--    atanh = undefined -- lensToUnnaryFunc atanhC
--    pi = undefined -- constLens pi


--class A a where
--  aa :: a -> a
--
--class B a where
--  bb :: a -> a
--
--class (A a, B a) => C a where
--  cc :: a -> a
--
--f :: (A a, B a) => a -> a
--f = aa . bb
--
--g :: C a => a -> a
--g = cc


