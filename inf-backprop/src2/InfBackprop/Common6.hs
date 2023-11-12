{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module InfBackprop.Common6 where

import Prelude ((.), id, fst, snd, uncurry, curry, ($), undefined, const, fmap, Functor)
import qualified Prelude as P
import NumHask ((*), (+), sin, cos, Additive, zero, Multiplicative, one, ExpField, exp, log, (**), sqrt, sinh, cosh, 
  Subtractive, negate, (-),
  Divisive, recip, (/), Field, AdditiveAction, (^), (^^), TrigField, two, Distributive, Integral)
-- import qualified NumHask as NH (sqrt, sinh, cosh)
import Data.Bifunctor (bimap)
import GHC.Base (Type)
import NumHask.Prelude (Float, flip)
import Prelude hiding (sinh, cosh, (*), (+), (-), negate, recip, exp, (^), (^^), (/), log, sqrt)
import Data.Void (Void, absurd)
import Data.Profunctor.Extra (Outer, (***), outer)
import Data.Profunctor (Profunctor, dimap, Strong, first', second', Costrong, unfirst, unsecond)

import Debug.SimpleExpr.Expr (number)
import Data.Basis2 (basis, End, Basis)
import Data.Linear (tupleDual, listDual)
import InfBackprop.LensD (LensD(LensD), view, update, unpackLensD, DFunc)
import InfBackprop.Tangent (T)
import Prelude.Tools (fork)

squareC :: (Additive x, Multiplicative x, T x ~ x) => -- , Diff x ~ x) =>
  DFunc x x
squareC = LensD $ \x -> (x * x, \dy -> two * x * dy)

square :: (Additive x, Multiplicative x, T x ~ x) =>
  DP x x x x
square = lensCtoP squareC

recipC :: (Divisive x, Subtractive x, T x ~ x) =>
  DFunc x x
recipC = LensD $ \x -> (recip x, \dy -> negate $ recip x^2 * dy)

instance (Divisive x, Subtractive dx, dx ~ x, T x ~ dx) =>
  Divisive (LensD dx x dx x) where
    recip = lensCtoP recipC


-- DP
type DP dx x dy y = forall p. Costrong p => p dx x -> p dy y

identity :: LensD a b a b
identity = LensD (, id)

lensCtoP :: LensD dx x dy y -> DP dx x dy y
-- lensCtoP (D v u) = unsecond . dimap (uncurry u) (fork id v)
lensCtoP l = unsecond . dimap (uncurry u) (fork id v) where
  v = view l
  u = update l

lensPtoC :: DP dx x dy y -> LensD dx x dy y
lensPtoC f = f identity




--class Const c b a where
--  const' :: c -> b -> a
--
--instance Const c b c where
--  const' = const
--
--instance (Additive db) =>
--  Const c (LensD dt t (Diff b) b) (LensD dt t (Diff c) c) where
--    const'= lensCtoP . constC





--tupleToLens :: Additive dt =>
--  (D dt t dx1 x1, D dt t dx2 x2) -> D dt t (dx1, dx2) (x1, x2)
---- tupleToLens (D v1 u1, D v2 u2) = D (\t -> (v1 t, v2 t)) (\t (dy1, dy2) -> u1 t dy1 + u2 t dy2)
--tupleToLens (D a1, D a2) = D $ \t -> let
--    (x1, dxt1) = a1 t
--    (x2, dxt2) = a2 t
--  in ((x1, x2), \(dx1, dx2) -> dxt1 dx1 + dxt2 dx2)
---- crossC3 (D v1 u1) (D v2 u2) = D (fork v1 v2) ()
--
--lensToTuple :: (Additive dx1, Additive dx2) =>
--  D dt t (dx1, dx2) (x1, x2) -> (D dt t dx1 x1, D dt t dx2 x2)
---- lensToTuple (D v u) = (D (fst . v) (\t dy1 -> u t (dy1, zero)), D (snd . v) (\t dy2 -> u t (zero, dy2)))
--lensToTuple (D a) = (D a1, D a2) where
--  a1 = \t -> let
--      ((x1, _), dxt) = a t
--    in (x1, \dx1 -> dxt (dx1, zero))
--  a2 = \t -> let
--      ((_, x2), dxt) = a t
--    in (x2, \dx2 -> dxt (zero, dx2))
--
--functionToLens :: (r -> D dt t dx x) -> D dt t (dx, r) (r -> x)
--functionToLens f = D $ \t ->
--  (\r -> view (f r) t, \(dx, r) -> update (f r) t dx)
--
----lensToTriple :: (Additive dx1, Additive dx2, Additive dx3) =>
----  D dt t (dx1, dx2, dx3) (x1, x2, x3) -> (D dt t dx1 x1, D dt t dx2 x2, D dt t dx3 x3)
----lensToTriple (D a) = (D a1, D a2, D a3) where
----  a1 = \t -> let
----      ((x1, _, _), dxt) = a t
----    in (x1, \dx1 -> dxt (dx1, zero, zero))
----  a2 = \t -> let
----      ((_, x2, _), dxt) = a t
----    in (x2, \dx2 -> dxt (zero, dx2, zero))
----  a3 = \t -> let
----      ((_, _, x3), dxt) = a t
----    in (x3, \dx3 -> dxt (zero, zero, dx3))


----class DiffFormIn a a' | a -> a' where
----  differentiableForm :: f -> (a -> b)
----
----instance DiffFormIn (a -> b) a b where
----  differentiableForm = id
----
----instance DiffFormIn (() -> b) a b where
----  differentiableForm = id
--
--
----differentiableForm :: (DiffFormIn a a', DiffFormOut b b') => (a -> b) -> (a' -> b')
----differentiableForm f = diffFormOut . f . diffFormIn
--
--
--instance (Additive y, Additive dx) =>
--  Additive (D dx x dy y) where
--    -- zero = D (const zero) (const zero)
--    zero = D $ const (zero, zero)
--    -- (D v1 u1) + (D v2 u2) = D (\x -> v1 x + v2 x) (\t -> u1 t + u2 t)
--    (D a1) + (D a2) = D $ \x -> let
--        (y1, dyx1) = a1 x
--        (y2, dyx2) = a2 x
--      in (y1 + y2, dyx1 + dyx2)
--
--instance (Subtractive a, Subtractive b) =>
--  Subtractive (a, b) where
--    negate = cross negate negate
--    (a, b) - (c, d) = (a - c, b - d)
--
--instance (Subtractive y, Subtractive dx) =>
--  Subtractive (D dx x dy y) where
--    -- negate (D v u) = D (negate v) (negate u)
--    negate (D a) = D (bimap negate negate . a)
--    -- (D v1 u1) - (D v2 u2) = D (\x -> v1 x - v2 x) (\t -> u1 t - u2 t)
--    D a1 - D a2 = D $ \x -> let
--        (y1, dyx1) = a1 x
--        (y2, dyx2) = a2 x
--      in (y1 - y2, dyx1 - dyx2)
--
--instance (MultiplicativeAction y dx dx, Additive dx, Multiplicative y, Differentiable x dx, Differentiable y dy) =>
--  Multiplicative (D dx x dy y) where
----    one = D (const one) (const zero)
--    one = constC one
----    (D v1 u1) * (D v2 u2) = D (\x -> v1 x * v2 x) (\x dy -> v2 x ..* u1 x dy + v1 x ..* u2 x dy)
--    D a1 * D a2 = D $ \x -> let
--        (y1, dyx1) = a1 x
--        (y2, dyx2) = a2 x
--      in (y1 * y2, \dy -> y2 ..* dyx1 dy + y1 ..* dyx2 dy)
--
--instance (Distributive a, Distributive b) =>
--  Distributive (a, b)
--
--instance (Distributive y, Distributive dx, MultiplicativeAction y dx dx, Differentiable x dx, Differentiable y dy) =>
--  Distributive (D dx x dy y) where
--
--
----derivative_ :: -- Differentiable y dy =>
----  (D dx x dx x -> D dx x dy y) -> x -> dy -> dx
----derivative_ f = update (f identity)
--
----derivativeOp :: -- Differentiable y dy =>
----  (D dx x dx x -> D dx x dy y) -> dy -> x -> dx
----derivativeOp f = flip $ update (f identity)
--
--derivativeOp :: (D dx x dx x -> D dx x dy y) -> x -> dy -> dx
----derivativeOp :: (D dt t dx x -> D dt t dy y) -> x -> dy -> dx
--derivativeOp f = update (f identity)

fullDerivativeOp :: (DFunc x x -> DFunc x y) -> x -> (y, T y -> T x)
fullDerivativeOp f = unpackLensD (f identity)

derivativeOp :: (DFunc x x -> DFunc x y) -> (y -> T y) -> x -> T x
derivativeOp f sb x = dyx (sb y) where
  (y, dyx) = unpackLensD (f identity) x

derivative :: Basis y =>
  (DFunc x x -> DFunc x y) ->
  x ->
  End y (T x)
derivative f = basis . fullDerivativeOp f

tupleDerivative :: forall x y1 y2. (
    Multiplicative (T y1),
    Multiplicative (T y2),
    Additive (T y1),
    Additive (T y2)
  ) =>
  (DFunc x x -> DFunc x (y1, y2)) -> x -> (T x, T x)
--tupleDerivative f x = (temp (const (one, zero)), temp (const (zero, one))) where
--  temp = (\sb -> derivativeOp f sb x) :: ((y1, y2) -> (Tangent y1, Tangent y2)) -> Tangent x
tupleDerivative f x = tupleDual dyx where
  (_, dyx) = fullDerivativeOp f x :: ((y1, y2), (T y1, T y2) -> T x)

--tripleDerivative :: (
--    Multiplicative (Tangent y1),
--    Multiplicative (Tangent y2),
--    Multiplicative (Tangent y3),
--    Additive (Tangent y1),
--    Additive (Tangent y2),
--    Additive (Tangent y3)
--  ) =>
--  (DFunc x x -> DFunc x (y1, y2, y3)) -> x -> (Tangent x, Tangent x, Tangent x)
--tripleDerivative f x =
--  (temp (const (one, zero, zero)), temp (const (zero, one, zero)), temp (const (zero, zero, one))) where
--    temp = \sb -> derivativeOp f sb x

--listDerivative :: forall x y. (
--    -- Functor f,
--    Multiplicative (T y),
--    Additive (T y)
--  ) =>
--  (DFunc x x -> DFunc x [y]) -> x -> [T x]
--listDerivative f x = listDual $ fullDerivativeOp f x --(y, dyx) where -- [dyx (basisList i) | (i, _) <- enumerate y] where
  --basisList i = [if i == j then one else zero | (j, _) <- enumerate y]
  --enumerate = zip [0..]
  -- (y, dyx) = fullDerivativeOp f x

listTupleDerivative :: forall x y1 y2. (
    Multiplicative (T y1),
    Multiplicative (T y2),
    Additive (T y1),
    Additive (T y2)
  ) =>
  (DFunc x x -> DFunc x [(y1, y2)]) -> x -> [(T x, T x)]
listTupleDerivative f x = [(dyx (basisList1 i), dyx (basisList2 i)) | (i, _) <- enumerate y] where
  basisList1 i = [if i == j then (one, zero) else zero | (j, _) <- enumerate y]
  basisList2 i = [if i == j then (zero, one) else zero | (j, _) <- enumerate y]
  enumerate = zip [0..]
  (y, dyx) = fullDerivativeOp f x :: ([(y1, y2)], [(T y1, T y2)] -> T x)
-- listTupleDerivative f x = listDual $ (cross id unitDual) $ fullDerivativeOp f x




----derivative :: (Differentiable y dy) =>
----  (D dx x dx x -> D dx x dy y) -> x -> dy'
----derivative f x = basis $ derivative_ f x
--
--
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

--instance (Additive y) =>
--  Additive (LensD dx x dy y) where







--instance (Field a, Field b) =>
--  Field (a, b)
--
--instance (Divisive x, MultiplicativeAction x dx dx, MultiplicativeAction x dt dt, Distributive x, Subtractive dx,
--  Subtractive x, Distributive dt, Subtractive dt, Differentiable t dt, Differentiable x dx) =>
--    Field (D dt t dx x) where
--
--expC :: (ExpField x, MultiplicativeAction x dx dx) =>
--  D dx x dx x
----expC = D exp (\x dy -> exp x ..* dy)
--expC = D $ \x -> let
--    expX = exp x
--  in (expX, (expX ..*))
--
--logC :: (ExpField x, MultiplicativeAction x dx dx) =>
--  D dx x dx x
--logC = D $ \x -> (log x, \dy -> recip x ..* dy)
--
--instance (ExpField x, MultiplicativeAction x dx dx, Distributive dx, Subtractive dx, Differentiable x dx) =>
--  ExpField (D dx x dx x) where
--    exp = lensCtoP expC
--    log = lensCtoP logC
--    (**) = undefined -- :: a -> a -> a
--
--sinhC :: (TrigField x, MultiplicativeAction x dx dx) => D dx x dx x
----sinhC = D sinh (\x dy -> cosh x ..* dy)
--sinhC = D $ \x -> (sinh x, \dy -> cosh x ..* dy)
--coshC :: (TrigField x, MultiplicativeAction x dx dx) => D dx x dx x
----coshC = D cosh (\x dy -> sinh x ..* dy)
--coshC = D $ \x -> (cosh x, \dy -> sinh x ..* dy)
--
--instance (TrigField x, MultiplicativeAction x dx dx, Distributive dx, Subtractive dx, Differentiable x dx) =>
--  TrigField (D dx x dx x) where
--    sinh = lensCtoP sinhC
--
--
--instance (TrigField a, TrigField b) =>
--  TrigField (a, b) where
--    sinh = cross sinh sinh
--    cosh = cross cosh cosh
--
--
--
--
---- Datatypes
--data L2Normed a = UnsafeL2Normed {x_ :: a, y_ :: a, z_ :: a}
--getX :: (Divisive a, Additive a, ExpField a) =>
--  L2Normed a -> a
--getX (UnsafeL2Normed x_ y_ z_) = x_ / sqrt (x_^2 + y_^2 + z_^2)
--getValues :: (Divisive a, Additive a, ExpField a) =>
--  L2Normed a -> (a, a, a)
--getValues (UnsafeL2Normed x_ y_ z_) = (x_ / d, y_ / d, z_ / d) where d = sqrt (x_^2 + y_^2 + z_^2)
--
--data TangentL2Normed a = UnsafeTangentL2Normed {tx_ :: a, ty_ :: a, tz_ :: a}
--getTangentValues :: (Divisive a, Additive a, ExpField a) =>
--  L2Normed a -> TangentL2Normed a -> (a, a, a)
--getTangentValues lna tlna = undefined
--
--instance (Divisive a, Additive a, ExpField a) =>
--  Differentiable (L2Normed a) (TangentL2Normed a) where
--    startBackprop lna = undefined
--
--
--
-- Examples
example_0_0 = cosh (0.0 :: Float) :: Float
-- example_0_1_ :: TrigField a => a -> Float -> a
-- derivativeOp :: (DFunc x x -> DFunc x y) -> (y -> T y) -> x -> T x
example_0_1_ = derivativeOp cosh :: (Float -> Float) -> Float -> Float -- :: Float
example_0_1 = derivative cosh :: Float -> Float
example_0_2_ = derivative (derivative cosh) :: Float -> Float
example_0_2 = unitDual . derivativeOp (unitDual . derivativeOp exp) :: Float -> Float


----example_0_1__ = functionToLens . (derivativeOp cosh) :: D dt t dx x -> D dt t (dx, Float) (Float -> x)
----example_0_2 = derivativeOpAuto cosh :: Float -> Float -> Float
----example_0_2 = (derivative . derivative) cosh (0.0 :: Float) :: Float
--
--example_1_0 = derivativeOp (tupleToLens . (\x -> (x, x))) :: Float -> (Float, Float) -> Float
---- example_1_1 = tupleDual (derivativeOp (tupleToLens . (\x -> (x, x)))) :: (Float -> Float, Float -> Float)
--example_1_2 = tupleDual . derivativeOp (tupleToLens . (\x -> (x, x))) :: (Float -> (Float, Float))
--
--
--example_2_0 = derivativeOp (uncurry (+) . lensToTuple) :: (Float, Float) -> Float -> (Float, Float)
--example_2_1 = unitDual . derivativeOp (uncurry (+) . lensToTuple) :: (Float, Float) -> (Float, Float)
--
--fff :: (Additive a, Multiplicative a) => (a, a, a) -> (a, a)
--fff (x, y, z) = (x + y + z, x * y * z)
--example_3_0 = derivativeOp (tupleToLens . fff . lensToTriple)
--  :: (Float, Float, Float) -> (Float, Float) -> (Float, Float, Float)
--example_3_1 = tupleDual . derivativeOp (tupleToLens . fff . lensToTriple)
--  :: (Float, Float, Float) -> ((Float, Float, Float), (Float, Float, Float))
--
--
---- example_4_0 = derivativeOp (uncurry (+) . lensToTuple)  -- :: (Float, Float) -> Float -> (Float, Float)
----example_4_1 = unitDual . derivativeOp (uncurry (+) . lensToTuple) :: (Float, Float) -> (Float, Float)