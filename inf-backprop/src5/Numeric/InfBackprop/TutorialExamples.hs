{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}


module Numeric.InfBackprop.TutorialExamples where

import Data.Type.Equality (type (~))
import GHC.Real (fromIntegral)
import Debug.DiffExpr (SymbolicFunc, unarrySymbolicFunc, BinarySymbolicFunc, 
    binarySymbolicFunc)
import Data.Stream (Stream, take, fromList)
import Debug.SimpleExpr (variable, SimpleExpr, simplify, SE)
import GHC.Show (show)
import GHC.Base ((<>), (.), Int, ($), Float, id, undefined)
import Numeric.InfBackprop (derivative, simpleDerivative, gradient, tupleDerivative, 
    BoxedVec, CT, derivativeOp, backprop, initDiff, 
    BackpropDiff(MkBackpropDiff), boxedVecDerivative, DerivativeWith)
import NumHask (Additive, (+), Multiplicative, (*), Distributive, zero, sum, TrigField, 
    sin, cos, ExpField)
import Data.Proxy (Proxy(Proxy))
import Data.Tuple (curry, uncurry)
import Numeric.InfBackprop.BackpropDiff (twoArgsDerivative, customArgDerivative, tupleArg, 
    BackpropDiff, derivative)
import GHC.TypeNats (KnownNat)
import qualified Data.Vector.Generic.Sized as DVGS
import Data.FiniteList (BoundedStream)


xVar :: SimpleExpr
xVar = variable "x"

streamF :: SymbolicFunc a => a -> Stream a
streamF x_ = fromList [unarrySymbolicFunc ("f_" <> show n) x_ | n <- [0 :: Int ..]]
_ = take 5 (streamF xVar)

streamF' :: SimpleExpr -> Stream SimpleExpr
streamF' = simplify . derivative streamF
_ = take 5 (streamF' xVar)

yVar :: SimpleExpr
yVar = variable "y"

h :: BinarySymbolicFunc a => a -> a -> a
h = binarySymbolicFunc "h"

-- h' = simplify . twoArgGradientTuple h :: SimpleExpr -> SimpleExpr -> (SimpleExpr, SimpleExpr)

f4 :: Additive a => (a, a) -> a
f4 (x, y) = x + y

_ = gradient @SE f xVar :: SE


-- BackpropDiff (SE, SE) -> (BackpropDiff (SE, SE) SE, BackpropDiff (SE, SE) SE)  
_ = gradient @SE f4 (xVar, yVar) :: (SE, SE)
--_ = gradient (Proxy @SE) f4 (xVar, yVar) :: (SE, SE)

--_ = gradient (Proxy @SE) (uncurry h) (xVar, yVar) :: (SE, SE)
_ = gradient @SE (uncurry h) (xVar, yVar) :: (SE, SE)

_ = twoArgsDerivative h xVar yVar :: (SE, SE)

f :: SymbolicFunc a => a -> a
f = unarrySymbolicFunc "f"
g :: SymbolicFunc a => a -> a
g = unarrySymbolicFunc "g"

--f_ :: a -> a
--f_ = undefined

temp0 :: forall a. (SymbolicFunc a, Multiplicative a, a ~ CT a) => a -> a -> a
--temp0 = backprop . (f :: BackpropDiff a a -> BackpropDiff a a) . (`MkBackpropDiff` id)
temp0 x = backprop $ f (MkBackpropDiff x id :: BackpropDiff a a)

temp :: forall a. (Multiplicative a, SymbolicFunc a, CT a ~ a) => a -> a -> a
temp = derivativeOp (f :: BackpropDiff a a -> BackpropDiff a a)

-- f'_ :: forall a. (SymbolicFunc a, Distributive a) => a -> CT a
-- f'_ :: GHC.Types.Any -> Numeric.InfBackprop.Tangent.Dual (Numeric.InfBackprop.Tangent.Tangent GHC.Types.Any)

-- f'_ :: forall a. (Distributive a, Multiplicative (CT a)) => a -> CT a
-- f'_ = (simpleDerivative :: (BackpropDiff a a -> BackpropDiff a a) -> a -> CT a) (f :: BackpropDiff a a -> BackpropDiff a a)

f' :: forall a. (SymbolicFunc a, Distributive a, a ~ CT a) => a -> a
f' = gradient @a (f :: BackpropDiff a a -> BackpropDiff a a)

fg :: (SymbolicFunc a, Multiplicative a) => (a, a) -> a
fg (x, y) = f x * g y

fg' :: forall a. (SymbolicFunc a, Distributive a, CT a ~ a) => (a, a) -> (a, a)
fg' = gradient @a @(BackpropDiff (a, a) a, BackpropDiff (a, a) a) fg

fg'' :: (SE, SE) -> ((SE, SE), (SE, SE))
fg'' = simplify . tupleDerivative (tupleDerivative fg)

fg''' :: (SE, SE) -> (((SE, SE), (SE, SE)), ((SE, SE), (SE, SE)))
fg''' = simplify . tupleDerivative (tupleDerivative (tupleDerivative fg))

-- _ = customArgDerivative tupleArg

--eNorm2 :: forall n a. (KnownNat n, Distributive a) => BoxedVec n a -> a
eNorm2 :: (Distributive a) => BoxedVec 3 a -> a
eNorm2 x = DVGS.foldl' (+) zero (x*x)

v :: BoxedVec 3 Float 
v = DVGS.fromTuple (1, 2, 3)

-- grad1 :: BoxedVec 3 Float
-- _ = gradient (Proxy @Float) eNorm2 (DVGS.fromTuple (1, 2, 3) :: BoxedVec 3 Float) :: BoxedVec 3 Float

-- BackpropDiff (BoxedVec n a) (BoxedVec n a) -> BoxedVec n (BackpropDiff (BoxedVec n a) a)
_ = gradient @Float eNorm2 (DVGS.fromTuple (1, 2, 3) :: BoxedVec 3 Float) :: BoxedVec 3 Float

eNorm2'_ :: BoxedVec 3 Float -> BoxedVec 3 Float
eNorm2'_ = gradient @Float eNorm2 

eNorm2' :: forall a. (Distributive a, CT a ~ a) => BoxedVec 3 a -> BoxedVec 3 a
eNorm2' = gradient @a @(BoxedVec 3 (BackpropDiff (BoxedVec 3 a) a)) eNorm2 

h_ :: BinarySymbolicFunc a => (a, a) -> a
h_ = uncurry h 

_ = gradient @SE h_ :: (SE, SE) -> (SE, SE)

take3Sum :: (Additive a) => Stream a -> a
take3Sum = sum . take 2

s :: SymbolicFunc a => a -> Stream a
s t = fromList [unarrySymbolicFunc ("s_" <> show n) t | n <- [0 :: Int ..]]

take3Sum' :: forall a. (Additive a, Distributive (CT a)) => Stream a -> BoundedStream (CT a)
take3Sum' = gradient @a take3Sum --versionum

_ = fromList [- fromIntegral n | n <- [0 :: Int ..]] :: Stream Float


sphericToVec :: (TrigField a) => (a, a) -> BoxedVec 3 a
sphericToVec (theta, phi) = 
    DVGS.fromTuple (sin theta * cos phi, sin theta * sin phi, cos theta)

-- sphericToVec_' :: (TrigField a, ExpField a, DerivativeWith a, a ~ CT a) => 
--     (a, a) -> BoxedVec 3 (a, a)
-- sphericToVec_' = BoxedVecGradient sphericToVec


sphericToVec' :: (TrigField a, ExpField a, DerivativeWith a, a ~ CT a) => 
    (a, a) -> BoxedVec 3 (a, a)
sphericToVec' = tupleDerivative sphericToVec

_ = simplify $ sphericToVec' (xVar, yVar) ::  BoxedVec 3 (SE, SE)



-- boxedVecDerivative