module Numeric.InfBackprop.TutorialExamples where

import NumHask (cosh, one, zero, Multiplicative, Distributive, TrigField, cosh, cos, sin, (+), Additive, Subtractive, 
  (-))
import Prelude (Float, (.), const, ($))
import Numeric.InfBackprop.DLens (derivativeOp, DLens(DLens), constC)
--import Numeric.InfBackprop.Cotangent (T1, CT, Dual)
import Numeric.InfBackprop.SmoothTuple (LensDMap, to)
import Control.PolyFunctor (BinnaryVectorizable, binarryFfmap)


--const' :: a -> a -> a
--const' = const

constOne :: Multiplicative a => a -> a
constOne = const one

const2x2 :: Distributive a => (a, a) -> ((a, a), (a, a))
const2x2 _ = ((one, zero), (zero, one))

-- Examples
example_0_0 = cosh (0.0 :: Float) :: Float
-- example_0_1_ :: TrigField a => a -> Float -> a
-- derivativeOp :: (DFunc x x -> DFunc x y) -> (y -> T y) -> x -> T x

example_0_1 = derivativeOp cosh :: (Float -> Float) -> Float -> Float -- :: Float
--example_0_2 = derivative cosh :: Float -> Float
--example_0_2_ :: Float
--example_0_2_ = derivative cosh (zero :: Float)
example_0_3 = derivativeOp cosh (const one) :: Float -> Float
example_0_3__ = derivativeOp cosh (const (one, one)) :: Float -> (Float, Float)

example_0_3_ = derivativeOp cosh constOne :: DLens Float Float Float Float -> DLens Float Float Float Float
example_0_3_0 :: Float -> Float
example_0_3_0 = derivativeOp cosh (const one :: Float -> Float)
example_0_3_1 :: DLens Float Float Float Float -> DLens Float Float Float Float
example_0_3_1 = derivativeOp cosh (const one :: DLens Float Float Float Float -> DLens Float Float Float Float)
--example_0_3_2 :: GHC.Types.Any -> Numeric.InfBackprop.Cotangent.Tangent1 GHC.Types.Any (Numeric.InfBackprop.Cotangent.Dual (Numeric.InfBackprop.Cotangent.Tangent1 GHC.Types.Any Float))
--example_0_3_2 :: forall a. a -> T1 a (CT a)
--example_0_3_2 = derivativeOp cosh (const one :: b -> T1 b (CT b))
--example_0_3_1 :: DLens Float Float (DLens Float Float Float Float) (DLens Float Float Float Float)
--example_0_3_3 = derivative cosh (zero :: DLens Float Float Float Float) -- -> DLens Float Float Float Float
--example_0_3_3 = derivative cosh :: a -> T1 a (CT a)
--example_0_3_4 = derivative cosh :: DLens Float Float Float Float -> DLens Float Float Float Float
example_0_4 = derivativeOp cosh (const one) (one :: Float) :: Float
example_0_5 = derivativeOp (derivativeOp cosh (const one)) :: (Float -> Float) -> Float -> Float
-- example_0_6 = derivativeOp (derivativeOp cosh ((constC :: Float -> DLens Float Float Float Float) one)) (const (one :: Float)) :: Float -> Float
example_0_7 = derivativeOp 
  (derivativeOp cosh (const one :: DLens Float Float Float Float -> DLens Float Float Float Float)) (const one)
  :: Float -> Float
example_0_8 = derivativeOp (derivativeOp cosh (const one :: a -> DLens Float Float Float Float)) (const one) 
  :: Float -> Float
example_0_9 = derivativeOp (derivativeOp cosh constOne) constOne :: Float -> Float
example_0_10 = derivativeOp (derivativeOp (derivativeOp cosh constOne) constOne) constOne :: Float -> Float
example_0_11 = derivativeOp (derivativeOp (derivativeOp (derivativeOp cosh constOne) constOne) constOne) constOne :: Float -> Float


--instance (Additive a, Additive b) =>
-- Additive (a, b) where
--   (x0, x1) + (y0, y1) = (x0 + y0, x1 + y1)
--   zero = (zero, zero)
--
--instance (Subtractive a, Subtractive b) =>
-- Subtractive (a, b) where
--   (x0, x1) - (y0, y1) = (x0 + y0, x1 + y1)
--
--instance (TrigField a, TrigField b) =>
-- TrigField (a, b) where
--   sin (x, y) = (sin x, sin y)
--   cos (x, y) = (cos x, cos y)

tupleToLens :: (Additive dt) =>
  (DLens dt t dx1 x1, DLens dt t dx2 x2) -> DLens dt t (dx1, dx2) (x1, x2)
tupleToLens (DLens a1, DLens a2) = DLens $ \t -> let
    (x1, dxt1) = a1 t
    (x2, dxt2) = a2 t
  in ((x1, x2), \(dx1, dx2) -> (binarryFfmap (+)) (dxt1 dx1) (dxt2 dx2))

lensToTuple :: (Additive dx0, Additive dx1) =>
  DLens dt t (dx0, dx1) (x0, x1) -> (DLens dt t dx0 x0, DLens dt t dx1 x1)
lensToTuple (DLens a) = (DLens a0, DLens a1) where
  a0 = \t -> let
      ((x0, _), dxt) = a t
    in (x0, \dx0 -> dxt (dx0, zero))
  a1 = \t -> let
      ((_, x1), dxt) = a t
    in (x1, \dx1 -> dxt (zero, dx1))

f1 :: TrigField a => a -> (a, a)
f1 x = (sin x, cos x)
example_1_1 = derivativeOp (tupleToLens . f1) const2x2 :: Float -> (Float, Float)
--example_1_1 = derivativeOp (to . f1) const2x2 :: Float -> (Float, Float)


--f :: TrigField a => (a, a) -> a
--f (x, y) = cos x + sin y 


--example_0_9 = derivativeOp (derivative cosh :: DLens Float Float Float Float -> DLens Float Float Float Float) :: (Float -> Float) -> Float -> Float
--example_0_9 = (derivative . derivative) cosh :: Float -> Float
--example_0_6 = (derivative . derivative . derivative) cosh :: Float -> Float



