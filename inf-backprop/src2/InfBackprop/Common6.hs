{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module InfBackprop.Common6 where

import Prelude ((.), id, fst, snd, uncurry, curry, ($), undefined, const, fmap, Functor, (<*>))
import Prelude hiding (sinh, cosh, (*), (+), (-), negate, recip, exp, (^), (^^), (/), log, sqrt, unzip, sum)
-- import qualified Prelude as P
import NumHask ((*), (+), sin, cos, Additive, zero, sum, Multiplicative, one, ExpField, exp, log, (**), sqrt, sinh, cosh,
  Subtractive, negate, (-),
  Divisive, recip, (/), Field, AdditiveAction, (^), (^^), TrigField, two, Distributive, Integral)
-- import qualified NumHask as NH (sqrt, sinh, cosh)
import Data.Bifunctor (bimap)
import GHC.Base (Type)
import NumHask.Prelude (Float, flip)
import Data.Void (Void, absurd)
import Data.Profunctor.Extra (Outer, (***), outer)
import Data.Profunctor (Profunctor, dimap, Strong, first', second', Costrong, unfirst, unsecond)

import Debug.SimpleExpr.Expr (number)
import Data.Basis3 (Basis, initBackProp, zeroBackProp, End, Vec, myBasisVector)
import Data.Linear (tupleDual, listDual)
import InfBackprop.LensD (LensD(LensD), view, update, unpackLensD, DFunc)
import InfBackprop.Tangent (T)
import Prelude.Tools (fork)
import Data.List.NonEmpty (unzip)
import GHC.TypeLits (KnownNat)
import Data.Vector.Generic.Sized (index, generate, Vector(..))
--import Data.Vector.Generic ((!))
import qualified Data.Vector.Fixed.Boxed as VFB
--import Data.Vector.Generic (generate)
import qualified Data.Vector.Fixed.Cont as DVFC -- (Peano, generate)
import qualified Data.Vector.Fixed as DVF --  Arity)
import Data.Proxy (Proxy(Proxy))


squareC :: (Additive x, Multiplicative x, T x ~ x) => -- , Diff x ~ x) =>
  DFunc x x
squareC = LensD $ \x -> (x * x, \dy -> two * x * dy)

--square :: (Additive x, Multiplicative x, T x ~ x) =>
--  DP x x x x
--square = lensCtoP squareC

recipC :: (Divisive x, Subtractive x, T x ~ x) =>
  DFunc x x
recipC = LensD $ \x -> (recip x, \dy -> negate $ recip x^2 * dy)

--instance (Distributive dx, y ~ dx) => 
--  Divisive (LensD dx x dy y) where
--    recip = lensCtoP recipC

instance (Divisive x, Subtractive dx, dx ~ x, T x ~ dx) =>
  Divisive (LensD dx x dx x) where
    recip = lensCtoP recipC

instance (Field x, x ~ T x) => 
  Field (LensD x x x x)

sinhC :: (TrigField x, x ~ T x) => DFunc x x
sinhC = LensD $ \x -> (sinh x, \dy -> cosh x * dy)

coshC :: (TrigField x, x ~ T x) => DFunc x x
coshC = LensD $ \x -> (cosh x, \dy -> sinh x * dy)

instance (TrigField x, x ~ T x) =>
  TrigField (LensD x x x x) where
    sinh = lensCtoP sinhC




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

temp :: LensD dt t (dx1, dx2) (x1, x2) -> (LensD dt t dx1 x1, LensD dt t dx2 x2)
temp = undefined

tupleArg :: (Additive dx1, Additive dx2) =>
  ((LensD dt t dx1 x1, LensD dt t dx2 x2) -> y) -> LensD dt t (dx1, dx2) (x1, x2) -> y
tupleArg f = f . lensToTuple

vecToLens :: (Additive dt, KnownNat n) =>
  Vec n (LensD dt t dx x) -> LensD dt t (Vec n dx) (Vec n x)
vecToLens a = LensD $ \t -> let
    (vec, dVecT) = unzip (fmap (\(LensD f) -> f t) a)
  in (vec, \dVec -> sum (dVecT <*> dVec))

instance -- (Additive (T x1), Additive (T x2)) => 
  ToLens (x1, x2) t where
    type ToLensMap (x1, x2) t = (DFunc t x1, DFunc t x2) -- -> DFunc t (x1, x2)
    toLens :: Additive (T t) => 
      (DFunc t x1, DFunc t x2) -> DFunc t (x1, x2) -- (DFunc t x1, DFunc t x1) -> DFunc t (x1, dx)
    toLens = tupleToLens

--temp1 :: Vec n (Int -> Int)
--temp1 = undefined

lensToVec :: forall dt t dx x n. (Additive dx, KnownNat n) =>
  LensD dt t (Vec n dx) (Vec n x) -> Vec n (LensD dt t dx x)
lensToVec (LensD a) = Data.Vector.Generic.Sized.generate $ \k ->
  LensD (\t -> (let (v, dvt :: Vec n dx -> dt) = a t in (index v k, dvt . myBasisVector k zero)))

vecNToLens :: (Additive dt, DVF.Arity n) =>
  VFB.Vec n (LensD dt t dx x) -> LensD dt t (VFB.Vec n dx) (VFB.Vec n x)
vecNToLens a = LensD $ \t -> let
    (vec, dVecT) = unzip (fmap (\(LensD f) -> f t) a)
  in (vec, \dVec -> sum (dVecT <*> dVec))

--lensToVecN :: forall dt t dx x n. (Additive dx, DVF.Arity n) =>
--  LensD dt t (VFB.Vec n dx) (VFB.Vec n x) -> VFB.Vec n (LensD dt t dx x)
--lensToVecN (LensD a) = DVFC.generate $ \k ->
--  LensD (\t -> (let (v, dvt :: VFB.Vec n dx -> dt) = a t in undefined)) -- (DVF.index v (Proxy k), dvt . myBasisVector k zero)))

lensToVec2 :: forall dt t dx x. (Additive dx) =>
  LensD dt t (VFB.Vec 2 dx) (VFB.Vec 2 x) -> VFB.Vec 2 (LensD dt t dx x)
lensToVec2 (LensD a) = DVF.convert (LensD a1, LensD a2) where
  a1 = \t -> let
      (DVF.convert -> (x1, _), dxt) = a t
    in (x1, \dx1 -> dxt (DVF.convert (dx1, zero)))
  a2 = \t -> let
      (DVF.convert -> (_, x2), dxt) = a t
    in (x2, \dx2 -> dxt (DVF.convert (zero, dx2)))  

--vec2Arg :: Additive dx =>
--  (VFB.Vec 2 (LensD dt t dx x_) -> y) -> LensD dt t (VFB.Vec 2 dx) (VFB.Vec 2 x_) -> y
vec2Arg :: Additive (T x) =>
  (VFB.Vec 2 (DFunc t x) -> y) -> DFunc t (VFB.Vec 2 x) -> y
vec2Arg f = f . lensToVec2

class ToLens (a :: Type) (t :: Type) where
  type ToLensMap a t :: Type
  toLens :: Additive (T t) => ToLensMap a t -> DFunc t a

lensToVec2Vec2 :: forall dt t dx x_. (Additive dx) =>
  LensD dt t (VFB.Vec 2 (VFB.Vec 2 dx)) (VFB.Vec 2 (VFB.Vec 2 x_)) -> VFB.Vec 2 (VFB.Vec 2 (LensD dt t dx x_))
lensToVec2Vec2 = fmap lensToVec2 . lensToVec2

vecNVecNToLens :: (Additive dt, DVF.Arity n) =>
  VFB.Vec n (VFB.Vec n (LensD dt t dx x)) -> LensD dt t (VFB.Vec n (VFB.Vec n dx)) (VFB.Vec n (VFB.Vec n x))
vecNVecNToLens = vecNToLens . fmap vecNToLens

--fmapPower n = case n of
--  0 -> id 



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

derivativeOp :: (DFunc x x -> DFunc x y) -> x -> (y, T y -> T x)
derivativeOp f = unpackLensD (f identity)

derivativeOp_ :: (DFunc x x -> DFunc x y) -> (y -> T y) -> x -> T x
derivativeOp_ f sb x = dyx (sb y) where
  (y, dyx) = unpackLensD (f identity) x

derivative :: Basis (T y) =>
  (DFunc x x -> DFunc x y) ->
  x ->
  End (T y) (T x)
derivative f x = initBackProp (snd (derivativeOp f x))

derivativeAndValue :: Basis (T y) =>
  (DFunc x x -> DFunc x y) ->
  x ->
  (y, End (T y) (T x))
derivativeAndValue f x = (y, initBackProp bp) where
  (y, bp) = derivativeOp f x



--tupleDerivative :: forall x y1 y2. (
--    Multiplicative (T y1),
--    Multiplicative (T y2),
--    Additive (T y1),
--    Additive (T y2)
--  ) =>
--  (DFunc x x -> DFunc x (y1, y2)) -> x -> (T x, T x)
----tupleDerivative f x = (temp (const (one, zero)), temp (const (zero, one))) where
----  temp = (\sb -> derivativeOp f sb x) :: ((y1, y2) -> (Tangent y1, Tangent y2)) -> Tangent x
--tupleDerivative f x = tupleDual dyx where
--  (_, dyx) = fullDerivativeOp f x :: ((y1, y2), (T y1, T y2) -> T x)

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

--listTupleDerivative :: forall x y1 y2. (
--    Multiplicative (T y1),
--    Multiplicative (T y2),
--    Additive (T y1),
--    Additive (T y2)
--  ) =>
--  (DFunc x x -> DFunc x [(y1, y2)]) -> x -> [(T x, T x)]
--listTupleDerivative f x = [(dyx (basisList1 i), dyx (basisList2 i)) | (i, _) <- enumerate y] where
--  basisList1 i = [if i == j then (one, zero) else zero | (j, _) <- enumerate y]
--  basisList2 i = [if i == j then (zero, one) else zero | (j, _) <- enumerate y]
--  enumerate = zip [0..]
--  (y, dyx) = fullDerivativeOp f x :: ([(y1, y2)], [(T y1, T y2)] -> T x)
---- listTupleDerivative f x = listDual $ (cross id unitDual) $ fullDerivativeOp f x




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
example_0_1_ = derivativeOp_ cosh :: (Float -> Float) -> Float -> Float -- :: Float
example_0_1 = derivative cosh :: Float -> Float
example_0_2 = (derivative . derivative) cosh :: Float -> Float
example_0_3 = (derivative . derivative . derivative) cosh :: Float -> Float


----example_0_1__ = functionToLens . (derivativeOp cosh) :: D dt t dx x -> D dt t (dx, Float) (Float -> x)
----example_0_2 = derivativeOpAuto cosh :: Float -> Float -> Float
----example_0_2 = (derivative . derivative) cosh (0.0 :: Float) :: Float

f1 = \x -> (x, x)
-- derivativeOp :: (DFunc x x -> DFunc x y) -> x -> (y, T y -> T x)
example_1_0 = derivativeOp (tupleToLens . f1) :: Float -> ((Float, Float), (Float, Float) -> Float)
--example_1_0_ :: forall t x. (Additive x, Additive (T t)) => 
--  DFunc t x -> ((DFunc t x, DFunc t x), (DFunc t x, DFunc t x) -> DFunc t x)
example_1_0_ = snd . derivativeOp (tupleToLens . f1) :: Float -> (Float, Float) -> Float
example_1_1 = derivative (tupleToLens . f1) :: Float -> (Float, Float)
example_1_2 = derivative (tupleToLens . f1) :: DFunc Float Float -> (DFunc Float Float, DFunc Float Float)
example_1_3 = derivative (tupleToLens . derivative (tupleToLens . f1)) :: Float -> (Float, Float)
example_1_4 = (derivative . tupleValue) f1 :: Float -> (Float, Float)
example_1_5 = ((derivative . tupleValue) . (derivative . tupleValue)) f1 :: Float -> (Float, Float)

--example_1_1_ = derivative (tupleToLens . f1) :: forall t x. Additive (T x) => DFunc t x -> (DFunc t x, DFunc t x)
--example_1_2 = derivative (derivative (tupleToLens . f1)) :: Float -> (Float, Float)
-- example_1_0 = derivativeOp (tupleToLens . (\x -> (x, x))) :: Float -> (Float, Float) -> Float
---- example_1_1 = tupleDual (derivativeOp (tupleToLens . (\x -> (x, x)))) :: (Float -> Float, Float -> Float)
--example_1_2 = tupleDual . derivativeOp (tupleToLens . (\x -> (x, x))) :: (Float -> (Float, Float))

f2 :: Additive x => (x, x) -> x
f2 = uncurry (+)
example_2_0 = derivativeOp (uncurry (+) . lensToTuple) :: (Float, Float) -> (Float, Float -> (Float, Float))
example_2_1 = derivative (uncurry (+) . lensToTuple) :: (Float, Float) -> (Float, Float)
example_2_2 = (derivative . tupleArg) f2 :: (Float, Float) -> (Float, Float)
-- example_2_0 = derivativeOp (uncurry (+) . lensToTuple) :: (Float, Float) -> Float -> (Float, Float)
--example_2_1 = unitDual . derivativeOp (uncurry (+) . lensToTuple) :: (Float, Float) -> (Float, Float)

type SmallVec = VFB.Vec

f3 :: Additive x => SmallVec 2 x -> x
f3 (DVF.convert -> (x, y)) = x + y
--example_3_0_ = f3 :: Additive x => VFB.Vec 2 (DFunc t x) -> DFunc t x
example_3_1 = derivative (f3 . lensToVec2) :: SmallVec 2 Float -> SmallVec 2 Float
example_3_1__ = customDerivative id lensToVec2 f3 :: SmallVec 2 Float -> SmallVec 2 Float

example_3_2 = (derivative . vec2Arg) f3 :: SmallVec 2 Float -> SmallVec 2 Float
example_3_2_ :: forall t. Additive (T t) => SmallVec 2 (DFunc t Float) -> SmallVec 2 (DFunc t Float)
example_3_2_ = (derivative . vec2Arg) f3

example_3_3 = derivative (vecNToLens . derivative (f3 . lensToVec2) . lensToVec2) :: SmallVec 2 Float -> SmallVec 2 (SmallVec 2 Float)
example_3_3_ :: forall t. Additive (T t) => SmallVec 2 (DFunc t Float) -> SmallVec 2 (SmallVec 2 (DFunc t Float))
example_3_3_ = derivative (vecNToLens . derivative (f3 . lensToVec2) . lensToVec2) -- :: VFB.Vec 2 Float -> VFB.Vec 2 (VFB.Vec 2 Float)
example_3_3__ = customDerivative vecNToLens lensToVec2 $ customDerivative id lensToVec2 f3 :: SmallVec 2 Float -> SmallVec 2 (SmallVec 2 Float)

example_3_4 = derivative (vecNVecNToLens . derivative (vecNToLens . derivative (f3 . lensToVec2) . lensToVec2) . lensToVec2) :: SmallVec 2 Float -> SmallVec 2 (SmallVec 2 (SmallVec 2 Float))
example_3_4__ = customDerivative vecNVecNToLens lensToVec2 $ customDerivative vecNToLens lensToVec2 $ customDerivative id lensToVec2 f3 :: SmallVec 2 Float -> SmallVec 2 (SmallVec 2 (SmallVec 2 Float))

--vecNToLens :: (Additive dt, DVF.Arity n) =>
--  VFB.Vec n (LensD dt t dx x) -> LensD dt t (VFB.Vec n dx) (VFB.Vec n x)

--derivative :: Basis (T y) =>
--  (DFunc x x -> DFunc x y) ->
--  x ->
--  End (T y) (T x)

--customDerivative :: Basis (T (v y)) =>
--  (v (DFunc (w x) y) -> DFunc (w x) (v y)) -> 
--  (DFunc (w x) (w x) -> w (DFunc (w x) x)) -> 
--  (w (DFunc (w x) x) -> v (DFunc (w x) y)) -> 
--  w x ->
--  End (T (v y)) (T (w x))
customDerivative :: Basis (T y) => 
  (b -> DFunc x y) -> 
  (DFunc x x -> a) -> 
  (a -> b) -> 
  x -> 
  End (T y) (T x)
customDerivative value arg f = derivative $ value . f . arg

-- idArg :: DFunc t (w x) -> w (DFunc t x)


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