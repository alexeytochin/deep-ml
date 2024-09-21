{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-export-lists #-}
--{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-imports -fno-warn-missing-export-lists #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Numeric.InfBackprop.TutorialExamples where

import Data.Type.Equality (type (~))
import NumHask (cosh, one, zero, Multiplicative, Distributive, TrigField, cosh, cos, sin, (+), Additive, Subtractive, 
  (-), (*), ExpField, FromIntegral)
--import Prelude ((.), const, ($), undefined, fst, snd, id)
--import Numeric.InfBackprop.DLens (DLens, derivativeOp, derivativeTuple)
--import Numeric.InfBackprop.DFunc (DFunc(MkDFunc), derivativeOp)
--import Numeric.InfBackprop.Basis (tupleToDFunc, tupleDerivative_, constOne, tupleDerivative__, dFuncToTuple, scalarDerivative, tupleGradient)
import Prelude.Tools (cross)
import NumHask.Extra10 ()
import Numeric.InfBackprop.DFunc2 (DFunc, derivativeOp, identity, pullback, constDFunc, stopDiff, constant)
--import Numeric.InfBackprop.DLens (DLens(MkDLens))
--import Numeric.InfBackprop.DLens ()
import Numeric.InfBackprop.Tangent (CT)
import Numeric.InfBackprop.DLens (Vec)
import Numeric.InfBackprop.DIfferentiable3 (Differentiable, tupleVal, longVecVal, initBackProp, lensToTuple, 
  lensToLongVec, tripleVal, Val, lensToTupleLongVec, derivative, HasDerivative, diff, customArgDerivative, gradient, 
  derivative2ArgsOverX, derivative2ArgsOverY, tupleDerivative, longVecDerivative, simpleDerivative)
import GHC.TypeNats (KnownNat)
import Data.Proxy (Proxy(Proxy))
import GHC.Base (Float, const, ($), undefined, (.), Int, (<>))
import NumHask.Extra (intPow, IntegralPower)
import Debug.SimpleExpr (variable, simplify, simplifyExpr, SimpleExpr)
import Data.Stream (Stream, take, fromList)
import Debug.DiffExpr (SymbolicFunc, unarrySymbolycFunc)
import GHC.Show (show, Show)
import qualified Data.Vector as DV
import qualified Data.Vector.Generic.Sized as DVGS
import Debug.Trace (trace)


type F = Float

--const' :: a -> a -> a
--const' = const


f0 :: forall a. (TrigField a, IntegralPower Int a) => a -> a
--f0 :: DFunc SimpleExpr SimpleExpr -> DFunc SimpleExpr SimpleExpr
f0 = sin . intPow 2

f0' :: forall a. (ExpField a, TrigField a, IntegralPower Int a, FromIntegral a Int, a ~ CT a) => 
  a -> CT a
--f0' :: SimpleExpr -> SimpleExpr
f0' = derivative f0 -- (f0 :: DFunc SimpleExpr SimpleExpr -> DFunc SimpleExpr SimpleExpr)

_ = simplify $ f0' (variable "x") :: SimpleExpr


constOne_ :: Multiplicative a => b -> a
constOne_ = const one

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
-- example_0_3__ = derivativeOp cosh (const (one, one)) :: Float -> (Float, Float)

example_0_3_ = derivativeOp cosh constOne_ :: DFunc Float Float -> DFunc Float Float
example_0_3_0 :: Float -> Float
example_0_3_0 = derivativeOp cosh (const one :: Float -> Float)
example_0_3_1 :: DFunc Float Float -> DFunc Float Float
example_0_3_1 = derivativeOp cosh (const one :: DFunc Float Float -> DFunc Float Float)
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
  (derivativeOp cosh (const one :: DFunc Float Float -> DFunc Float Float)) (const one)
  :: Float -> Float
example_0_8 = derivativeOp (derivativeOp cosh (const one :: a -> DFunc Float Float)) (const one)
  :: Float -> Float
example_0_9 = derivativeOp (derivativeOp cosh constOne_) constOne_ :: Float -> Float
example_0_10 = derivativeOp (derivativeOp (derivativeOp cosh constOne_) constOne_) constOne_ :: Float -> Float
example_0_11 = derivativeOp (derivativeOp (derivativeOp (derivativeOp cosh constOne_) constOne_) constOne_) constOne_ :: Float -> Float

bpf :: DFunc t F -> t -> CT t
bpf = initBackProp (Proxy @F)

example_1_0 = cosh identity :: DFunc Float Float
example_1_1 = bpf (cosh identity) :: Float -> Float



_ = simpleDerivative cosh :: F -> F
_ = simpleDerivative $ simpleDerivative cosh :: F -> F
_ = simpleDerivative $ simpleDerivative $ simpleDerivative cosh :: F -> F
_ = simpleDerivative $ simpleDerivative $ simpleDerivative $ simpleDerivative cosh :: F -> F


-- Two values
f2 :: TrigField a => a -> (a, a)
f2 x = (sin x, cos x)

--initLongVecTupleVal :: (Multiplicative (CT a0), Multiplicative (CT a1)) =>
--  Differentiable t (LongVec n0 (DFunc t a0), LongVec n1 (DFunc t a1))
--initLongVecTupleVal = tupleVal (longVecVal (initBackProp (Proxy @Float)), longVecVal (initBackProp (Proxy @Float)))


--tupleDerivative :: (Multiplicative (CT a0), Multiplicative (CT a1)) =>
--  (DFunc a a -> (DFunc a a0, DFunc a a1)) -> a -> (CT a, CT a)
--tupleDerivative :: forall a b. (Multiplicative (CT b)) =>
--  (DFunc a a -> (DFunc a b, DFunc a b)) -> 
--  a -> 
--  (CT a, CT a)
--tupleDerivative f = tupleVal (initBackProp (Proxy @b), initBackProp (Proxy @b)) (f identity)
tupleSimpleDerivative :: forall a b0 b1. (Multiplicative (CT b0), Multiplicative (CT b1)) =>
  (DFunc a a -> (DFunc a b0, DFunc a b1)) -> 
  a -> 
  (CT a, CT a)
tupleSimpleDerivative f = tupleVal (initBackProp (Proxy @b0), initBackProp (Proxy @b1)) (f identity)


-- temp0 = initBackProp . sin

_ = f2 identity :: (DFunc Float Float, DFunc Float Float)
_ = tupleVal (bpf, bpf) (f2 identity) :: Float -> (Float, Float)
-- example_2_2 = tupleVal (initBackProp (Proxy @F), initBackProp (Proxy @F)) (f2 identity) :: DFunc Float Float -> (DFunc Float Float, DFunc Float Float)
-- example_2_3 = tupleVal (initBackProp, initBackProp) (tupleVal (initBackProp, initBackProp) (f2 identity) identity) :: Float -> (Float, Float)
--example_1_2 = (tupleVal (initBackProp, initBackProp)) (tupleVal (initBackProp, initBackProp)) (f2 identity) :: Float -> (Float, Float)

_ = tupleSimpleDerivative f2 :: Float -> (Float, Float)
_ = tupleSimpleDerivative $ tupleSimpleDerivative f2 :: Float -> (Float, Float)
_ = tupleSimpleDerivative $ tupleSimpleDerivative $ tupleSimpleDerivative f2 :: Float -> (Float, Float)
_ = tupleSimpleDerivative $ tupleSimpleDerivative $ tupleSimpleDerivative $ tupleSimpleDerivative f2 :: Float -> (Float, Float)

_ = derivative f2 :: Float -> (Float, Float)
_ = derivative $ derivative f2 :: Float -> (Float, Float)
_ = derivative $ derivative $ derivative f2 :: Float -> (Float, Float)




longVecSimpleDerivative :: forall a n. (Multiplicative (CT a)) =>
  (DFunc a a -> Vec n (DFunc a a)) -> a -> Vec n (CT a)
longVecSimpleDerivative f = longVecVal (initBackProp (Proxy @a)) (f identity)

tupleLongVecDerivative :: forall a b n0 n1. (Multiplicative (CT b)) =>
  (DFunc a a -> (Vec n0 (DFunc a b), Vec n1 (DFunc a b))) ->
  a ->
  (Vec n0 (CT a), Vec n1 (CT a))
tupleLongVecDerivative f = tupleVal (longVecVal (initBackProp (Proxy @b)), longVecVal (initBackProp (Proxy @b))) (f identity)

f3 :: a -> (Vec n0 a, Vec n1 a)
f3 = undefined
example_3_1 = tupleLongVecDerivative f3 :: Float -> (Vec n0 Float, Vec n1 Float)
example_3_2 = tupleLongVecDerivative $ tupleLongVecDerivative f3 :: Float -> (Vec n0 Float, Vec n1 Float)
example_3_3 = tupleLongVecDerivative $ tupleLongVecDerivative $ tupleLongVecDerivative f3 :: Float -> (Vec n0 Float, Vec n1 Float)
example_3_4 = tupleLongVecDerivative $ tupleLongVecDerivative $ tupleLongVecDerivative $ tupleLongVecDerivative f3 :: Float -> (Vec n0 Float, Vec n1 Float)


f31 :: SymbolicFunc a => a -> Stream a
f31 x = fromList [unarrySymbolycFunc ("f_" <> show n) x | n <- [0 :: Int ..]]
_ = simplify . derivative f31 :: SimpleExpr -> Stream SimpleExpr

v :: SymbolicFunc a => a -> DVGS.Vector DV.Vector 3 a
v x = DVGS.fromTuple (unarrySymbolycFunc "v_x" x, unarrySymbolycFunc "v_y" x, unarrySymbolycFunc "v_z" x)


--example_1_1 :: (TrigField a) => a -> (a, a)
--example_1_1 = tupleVal initBackProp initBackProp . f2

--_ = tupleDerivative_ (tupleToDFunc . f2) :: Float -> (Float, Float)
----_ = tupleDerivative_ (tupleToDFunc . f2) :: DFunc Float Float -> (DFunc Float Float, DFunc Float Float)
--_ = tupleDerivative_ (tupleToDFunc . f2) :: DFunc (Float, Float) Float -> (DFunc (Float, Float) Float, DFunc (Float, Float) Float)
--_ = tupleDerivative_ (tupleToDFunc . tupleDerivative_ (tupleToDFunc . f2)) :: Float -> (Float, Float)
--_ = tupleDerivative__ $ tupleDerivative__ f2 :: Float -> (Float, Float)
--_ = tupleDerivative__ $ tupleDerivative__ $ tupleDerivative__ f2 :: Float -> (Float, Float)
--_ = tupleDerivative__ $ tupleDerivative__ $ tupleDerivative__ $ tupleDerivative__ f2 :: Float -> (Float, Float)
--_ = tupleDerivative__ $ tupleDerivative__ $ tupleDerivative__ $ tupleDerivative__ $ tupleDerivative__ f2 :: Float -> (Float, Float)


-- Two args
f4 :: Additive a => (a, a) -> a
f4 (x, y) = x + y

derivativeTuple :: forall a0 a1 b. (Additive (CT a0), Additive (CT a1), Multiplicative (CT b)) =>
  ((DFunc (a0, a1) a0, DFunc (a0, a1) a1) -> DFunc (a0, a1) b) ->
  (a0, a1) ->
  (CT a0, CT a1)
derivativeTuple f = initBackProp (Proxy @b) ((f . lensToTuple) identity)

derivativeXTuple :: forall a0 a1 b. (Additive (CT a0), Multiplicative (CT b)) =>
  ((DFunc a0 a0, DFunc a0 a1) -> DFunc a0 b) ->
  (a0, a1) ->
  CT a0
derivativeXTuple f (x0, x1) = initBackProp (Proxy @b) (f (identity, constDFunc x1)) x0


derivativeLongVec :: forall a n. (Distributive (CT a), KnownNat n) =>
  (Vec n (DFunc (Vec n a) a) -> DFunc (Vec n a) a) ->
  Vec n a ->
  Vec n (CT a)
derivativeLongVec f = initBackProp (Proxy @a) ((f . lensToLongVec) identity)

_ = f4 :: (DFunc Float Float, DFunc Float Float) -> DFunc Float Float
_ = f4 . lensToTuple :: DFunc (Float, Float) (Float, Float) -> DFunc (Float, Float) Float
_ = pullback ((f4 . lensToTuple) identity) :: (Float -> Float) -> (Float, Float) -> (Float, Float)
_ = pullback ((f4 . lensToTuple) identity) (const one) :: (Float, Float) -> (Float, Float)
_ = bpf ((f4 . lensToTuple) identity) :: (Float, Float) -> (Float, Float)

--tupleDerivative :: (Multiplicative (CT a)) =>
--  (DFunc a a -> (DFunc a a, DFunc a a)) -> a -> (CT a, CT a)
tripleDerivativeTuple :: forall a. (Distributive (CT a)) =>
  ((DFunc (a, a) a, DFunc (a, a) a) -> (DFunc (a, a) a, DFunc (a, a) a, DFunc (a, a) a)) ->
  (a, a) ->
  ((CT a, CT a), (CT a, CT a), (CT a, CT a))
tripleDerivativeTuple f = tripleVal (initBackProp (Proxy @a), initBackProp (Proxy @a), initBackProp (Proxy @a)) ((f . lensToTuple) identity)

derivativeTupleLongVec :: forall a n0 n1. (Distributive (CT a), KnownNat n0, KnownNat n1) =>
  (
    (
      Vec n0 (DFunc (Vec n0 a, Vec n1 a) a),
      Vec n1 (DFunc (Vec n0 a, Vec n1 a) a)
    ) -> 
    DFunc (Vec n0 a, Vec n1 a) a
  ) -> 
  (Vec n0 a, Vec n1 a) ->
  (Vec n0 (CT a), Vec n1 (CT a))
derivativeTupleLongVec f = initBackProp (Proxy @a) ((f . lensToTupleLongVec) identity)





_ = tupleDerivative f4 :: (F, F) -> (F, F)
_ = tupleDerivative $ tupleDerivative f4 :: (F, F) -> ((F, F), (F, F))
_ = tupleDerivative $ tupleDerivative $ tupleDerivative f4 :: (F, F) -> (((F, F), (F, F)), ((F, F), (F, F)))
_ = tupleDerivative $ tupleDerivative $ tupleDerivative $ tupleDerivative f4 :: (F, F) -> ((((F, F), (F, F)), ((F, F), (F, F))), (((F, F), (F, F)), ((F, F), (F, F))))

f4__ :: forall a n. KnownNat n => Vec n a -> a
f4__ = undefined

example_3_10 :: forall n. KnownNat n => Vec n F -> Vec n F
example_3_10 = longVecDerivative f4__
example_3_11 :: forall n. KnownNat n => Vec n F -> Vec n (Vec n F)
example_3_11 = longVecDerivative $ longVecDerivative f4__
example_3_12 :: forall n. KnownNat n => Vec n F -> Vec n (Vec n (Vec n F))
example_3_12 = longVecDerivative $ longVecDerivative $ longVecDerivative f4__

f4_ :: (a, a) -> Vec n a
f4_ = undefined

--_ = tupleDerivative f4_ :: (F, F) -> (LongVec n F, LongVec n F)
_ = tupleDerivative f4_ :: (F, F) -> Vec n (F, F)
_ = tupleDerivative $ tupleDerivative f4_ :: (F, F) -> Vec n ((F, F), (F, F))

f4___ :: forall a n. Vec n a -> (a, a)
f4___ = undefined
example_3_20 :: KnownNat n => Vec n F -> (Vec n F, Vec n F)
example_3_20 = longVecDerivative f4___ -- :: forall n. KnownNat n => LongVec n F -> (LongVec n F, LongVec n F)
example_3_21 :: KnownNat n => Vec n F -> (Vec n (Vec n F), Vec n (Vec n F))
example_3_21 = longVecDerivative $ longVecDerivative f4___
example_3_22 :: KnownNat n => Vec n F -> (Vec n (Vec n (Vec n F)), Vec n (Vec n (Vec n F)))
example_3_22 = longVecDerivative $ longVecDerivative $ longVecDerivative f4___







    
_ = customArgDerivative lensToTuple (Proxy @F) f4 :: (F, F) -> (F, F)
--example_5_1 = customArgDerivative lensToTuple (f4 :: (DFunc (F, F) F, DFunc (F, F) F) -> DFunc (F, F) F)
_ = gradient (Proxy @F) f4 :: (F, F) -> (F, F)



f5 :: (a, a) -> (a, a, a)
f5 = undefined
_ = tripleDerivativeTuple f5 :: (Float, Float) -> ((Float, Float), (Float, Float), (Float, Float))




-- example_1 :: DLens Float Float (Float, Float) (Float, Float) -> DLens Float Float Float Float
--_ = dFuncToTuple :: DFunc Float (Float, Float) -> (DFunc Float Float, DFunc Float Float)
---- _ = f4 . dFuncToTuple :: DFunc Float (Float, Float) -> DFunc Float Float
--_ = dFuncToTuple ::
--  DFunc (Float, Float) (Float, Float) ->
--  (
--    DFunc (Float, Float) Float,
--    DFunc (Float, Float) Float
--  )


--_ = f4 :: (DFunc (Float, Float) Float, DFunc (Float, Float) Float) -> DFunc (Float, Float) Float
--_ = tupleGradient f4 :: (Float, Float) -> (Float, Float)





-- _ = derivativeY (+) x y
-- 0+1
_ = derivative2ArgsOverX (+) :: F -> F -> F
_ = derivative2ArgsOverY (+) :: F -> F -> F

--DifferentiableConst :: DFunc t a -> DFunc t b

--temp10 = constant (Proxy @F) (42 :: F)

_ = derivative (constant (Proxy @F) (42 :: F)) :: F -> F
_ = derivative $ derivative (constant (Proxy @F) (42 :: F)) :: F -> F
_ = derivative $ derivative $ derivative (constant (Proxy @F) (42 :: F)) :: F -> F

_ = derivative (\x -> stopDiff (42 :: F) * x) :: Float -> Float

_ = derivative (\x -> stopDiff (2 :: F) * cosh x) :: F -> F
_ = derivative $ derivative (\x -> stopDiff (2 :: F) * cosh x) :: F -> F
_ = derivative $ derivative $ derivative (\x -> stopDiff (2 :: F) * cosh x) :: F -> F

-- | ggg
-- \[
--    \left.
--      \frac{d}{dx}
--      \left(
--        x
--          \left(
--            \left.
--              \frac{d}{dy}
--              \left(
--                x + y
--              \right)
--            \right|_{y=1}
--          \right)
--      \right)
--    \right|_{x=1}
--    = 1
-- \] 



_ = (\x -> x * derivative2ArgsOverY (+) x 1) :: F -> F

fSiskindPearlmutter :: F
fSiskindPearlmutter = derivative (\x -> x * derivative2ArgsOverY (+) x (stopDiff (1 :: F))) 1
-- fSiskindPearlmutter = undefined -- derivative (\x -> x * derivativeY (+) x (stopDiff (1 :: Float))) (1 :: Float)


--traceF :: (Show a, SymbolicFunc a) => a -> a
--traceF x = trace ("\n:> Calculating f over " <> show x) $ unarrySymbolycFunc "f" x 
traceF :: SymbolicFunc a => a -> a
traceF x = trace "\n:> Calculating f" $ unarrySymbolycFunc "f" x 
g, h :: SymbolicFunc a => a -> a
g = unarrySymbolycFunc "g"
h = unarrySymbolycFunc "h"
xVar = variable "x" :: SimpleExpr

subexpF :: (SymbolicFunc a) => a -> (a, a)
subexpF x = (g y, h y) where y = traceF x

_ = derivative subexpF xVar