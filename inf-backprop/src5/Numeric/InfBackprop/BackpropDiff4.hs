{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveGeneric #-}

module Numeric.InfBackprop.BackpropDiff4 (
    BackpropDiff(MkBackpropDiff, value, backprop),
    initDiff,
    call,
    constDiff,
    derivativeOp,
    derivative,
    customArgDerivative,
    customValGradient,
    gradient,
    fieldDerivative,
    simpleDerivative,
    tupleArg,
    tupleVal,
) where


import Numeric.InfBackprop.Tangent (Tangent, Dual, CT, LongVec, BoxedVec)
import GHC.Base (id, (.), ($), const, undefined, flip, Type, Maybe(Just, Nothing), fmap, Float, Functor, Applicative, 
  pure, (<*>))
import Data.Tuple (fst, snd, uncurry, curry)
import NumHask ((*), (+), sin, cos, Additive, zero, Multiplicative, one, ExpField, exp, log, (**), sqrt, sin, cos, sinh,
    asin, acos, cosh, atan, atan2, asinh, acosh, atanh, pi,
    Subtractive, negate, (-),
    Divisive, recip, (/), Field, AdditiveAction, (^), (^^), TrigField, two, Distributive, Integral, fromIntegral,
    FromIntegral
  )
import Control.Applicative ((<$>), (<*>))
import Data.Type.Equality (type (~))
import Prelude.Tools (cross, cross3)
import GHC.Generics (Generic, type (:.:) (unComp1))
import Data.Primitive (Prim)
import Debug.Traced (Traced(MkTraced))
import NumHask.Extra (IntegralPower, integralPow)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Stream as DS
import Debug.SimpleExpr (SimpleExpr)
import Data.Finite (Finite)
import GHC.TypeNats (KnownNat)
import Data.Vector.Generic.Base (Vector(basicUnsafeFreeze, basicUnsafeThaw, basicLength, basicUnsafeSlice, basicUnsafeIndexM))
import qualified Data.Vector as DV
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Vector.Generic.Base as DVGB
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Primitive as DVP
import qualified Data.Vector.Generic.Mutable.Base as DVGBM
import qualified Data.Vector.Generic.Mutable as DVGM
import NumHask.Extra (vecBasis, longVecSum)
import Prelude.Tools (curry3, uncurry3)
import qualified Data.FiniteList as DF
import Control.ExtendableMap (ExtandableMap, extendMap)
import Data.Coerce (coerce)
import Data.Function (on)
import Data.Stream (Stream)
import Data.Tuple.Extra ((***))
import Data.FiniteList (BoundedStream)
import GHC.TypeLits (KnownChar)


data BackpropDiff t a = 
    MkBackpropDiff {value :: a, backprop :: CT a -> CT t} deriving (Generic)

initDiff :: a -> BackpropDiff a a
initDiff x = MkBackpropDiff x id

call :: (BackpropDiff a a -> BackpropDiff a b) -> a -> b
call f = value . f . initDiff

withField :: forall a b. BackpropDiff a b -> (b -> CT b) -> CT a
withField (MkBackpropDiff value_ backprop_) v = backprop_ (v value_)

type instance Tangent (BackpropDiff a b) = BackpropDiff a (Tangent b)
type instance Dual (BackpropDiff a b) = BackpropDiff a (Dual b)

constDiff :: Additive (CT t) => 
  a -> BackpropDiff t a
constDiff x = MkBackpropDiff x (const zero)

derivativeOp :: (BackpropDiff a a -> BackpropDiff a b) ->
  a ->
  CT b ->
  CT a
derivativeOp f = backprop . f . initDiff

fieldDerivative ::
  (BackpropDiff a a -> BackpropDiff a b) ->
  a ->
  (b -> CT b) -> 
  CT a
fieldDerivative f = withField . f . initDiff

-- | Derivative scalar -> scalar
simpleDerivative :: forall a b. (Multiplicative (CT b)) =>
  (BackpropDiff a a -> BackpropDiff a b) ->
  a ->
  CT a
simpleDerivative f x = fieldDerivative f x (const one) 
-- simpleDerivative f = withField @a @b (const  one) . f . initDiff


type DiffArg a b c = BackpropDiff a b -> c

-- class DifferentiableArg a b c | c -> a b where
--   startDiff :: DiffArg a b c -- BackpropDiff a b -> c

class DifferentiableArg a b | b -> a where
  startDiff :: a -> b

-- instance DifferentiableArg a b (BackpropDiff a b) where
--   startDiff = id

instance DifferentiableArg (BackpropDiff a b) (BackpropDiff a b) where
  startDiff = id


-- instance DifferentiableArg a (BackpropDiff a a) where
--   startDiff = initDiff


-- type family DiffOutput b :: Type
-- type DifferentiableVal b = b -> DiffOutput a b

-- type DiffOutput a (BackpropDiff a b)


class Differentiable a where
  startBackprop :: BackpropDiff t a -> CT t

temp :: Tangent Float
temp = 1

instance Differentiable Float where
  -- startBackprop :: forall t. BackpropDiff t Float -> CT t
  -- startBackprop (MkBackpropDiff _ bp) = bp 1
  startBackprop x = withField x (const 1 :: Float -> CT Float)

instance Differentiable SimpleExpr where
  startBackprop x = withField x (const one)


class DifferentialVal a b | a -> b where
  autoVal :: a -> b

val :: forall a b. Multiplicative (CT b) => 
  BackpropDiff a b -> CT a
val (MkBackpropDiff _ bp) = bp one

instance (Multiplicative (CT b), c ~ CT a) =>
  DifferentialVal (BackpropDiff a b) c where
    autoVal :: BackpropDiff a b -> c
    autoVal = val 
    -- autoVal (MkBackpropDiff _ bp) = bp one
    -- getDiff = withField (const one)


customDerivative ::
  (BackpropDiff a a -> b) ->
  (c -> d) ->
  (b -> c) ->
  a -> d
customDerivative arg val_ f = val_ . f . arg . initDiff

customArgDerivative :: DifferentialVal c d => 
  (BackpropDiff a a -> b) ->
  (b -> c) ->
  a -> d
customArgDerivative arg = customDerivative arg autoVal

customValGradient :: forall a b c d. DifferentiableArg (BackpropDiff a a) b =>
  (c -> d) ->
  (b -> c) ->
  a -> d
customValGradient = customDerivative startDiff

derivative :: DifferentialVal c d =>
  (BackpropDiff a a -> c) ->
  a -> 
  d
derivative = customArgDerivative id

gradient :: forall e a b. (DifferentiableArg (BackpropDiff a a) b, Multiplicative (CT e)) =>
  (b -> BackpropDiff a e) ->
  a -> 
  CT a
gradient = customValGradient val

-- Tuples
packTuple :: (Additive (CT a0), Additive (CT a1)) =>
  BackpropDiff t (a0, a1) -> (BackpropDiff t a0, BackpropDiff t a1)
packTuple (MkBackpropDiff (x0, x1) bpc) = (
    MkBackpropDiff x0 (\cy -> bpc (cy, zero)),
    MkBackpropDiff x1 (\cy -> bpc (zero, cy))
  )

tupleArg :: (Additive (CT b0), Additive (CT b1)) =>
  DiffArg a b0 c0 -> DiffArg a b1 c1 -> DiffArg a (b0, b1) (c0, c1)
-- tupleArg :: (BackpropDiff a b0 -> c0) -> (BackpropDiff a b1 -> c1) -> (BackpropDiff a (b0, b1) -> (c0, c1))
tupleArg f0 f1 = cross f0 f1 . packTuple

-- instance (DifferentiableArg a b0 c0, DifferentiableArg a b1 c1, Additive (CT b0), Additive (CT b1)) => 
--   DifferentiableArg a (b0, b1) (c0, c1) where
--     startDiff :: BackpropDiff a (b0, b1) -> (c0, c1) -- DiffArg a (BoxedVec n b) (BoxedVec n c)
--     startDiff = tupleArg startDiff startDiff

instance (
    DifferentiableArg a b0, 
    DifferentiableArg a b1, 
    Additive (CT b0), 
    Additive (CT b1)
  ) => 
  DifferentiableArg (BackpropDiff a (b0, b1)) (BackpropDiff a b0, BackpropDiff a b1) where
    startDiff :: BackpropDiff a (b0, b1) -> (BackpropDiff a b0, BackpropDiff a b1) -- DiffArg a (BoxedVec n b) (BoxedVec n c)
    startDiff = tupleArg startDiff startDiff

-- instance (DifferentiableArg a b0, DifferentiableArg a b1, Additive (CT a0), Additive (CT a1)) => 
--   DifferentiableArg (a0, a1) (BackpropDiff (a0, a1) a0, BackpropDiff (a0, a1) a1) where
--     startDiff :: (a0, a1) -> (BackpropDiff (a0, a1) a0, BackpropDiff (a0, a1) a1) -- DiffArg a (BoxedVec n b) (BoxedVec n c)
--     startDiff = tupleArg startDiff startDiff


-- DifferentiableArg a b0 -> DifferentiableArg a b1 -> DifferentiableArg a (b0, b1)

unpackTuple :: Additive (CT a) => 
  (BackpropDiff a b0, BackpropDiff a b1) -> BackpropDiff a (b0, b1)
unpackTuple (MkBackpropDiff v0 bp0, MkBackpropDiff v1 bp1) = 
  MkBackpropDiff (v0, v1) (uncurry (+) . (bp0 *** bp1))

tupleVal :: (a0 -> b0) -> (a1 -> b1) -> (a0, a1) -> (b0, b1)
tupleVal = cross


instance (DifferentialVal a0 b0, DifferentialVal a1 b1) =>
  DifferentialVal (a0, a1) (b0, b1) where
    autoVal :: (a0, a1) -> (b0, b1)
    autoVal = tupleVal autoVal autoVal




-- BoxedVec

packBoxedVec :: (Additive (CT b), KnownNat n) => 
  BackpropDiff a (BoxedVec n b) -> BoxedVec n (BackpropDiff a b)
packBoxedVec (MkBackpropDiff vec bpc) = DVGS.generate $ \k ->
  MkBackpropDiff (DVGS.index vec k) (bpc . vecBasis k zero)


unpackBoxedVec :: (Additive (CT t), KnownNat n) => 
  BoxedVec n (BackpropDiff t a) -> BackpropDiff t (BoxedVec n a) 
-- unpackBoxedVec :: DifferentiableVal (BoxedVec n (BackpropDiff a b)) a (BoxedVec n b)  
unpackBoxedVec vec = MkBackpropDiff 
  (fmap value vec) 
  (longVecSum . (fmap backprop vec <*>))

boxedVecArg :: (Additive (CT b), KnownNat n) => 
  DiffArg a b c -> DiffArg a (BoxedVec n b) (BoxedVec n c)
-- boxedVecArg :: (BackpropDiff a b -> c) -> BackpropDiff (BoxedVec n a) b -> (BoxedVec n c)
boxedVecArg f = fmap f . packBoxedVec -- packBoxedVec

-- instance DifferentiableArg a b c => 
--   DifferentiableArg a (BoxedVec n b) (BoxedVec n c) where
--     startDiff :: BackpropDiff a (BoxedVec n b) -> BoxedVec n c -- DiffArg a (BoxedVec n b) (BoxedVec n c)
--     startDiff = boxedVecArg startDiff

instance (DifferentiableArg a b, Additive (CT b), KnownNat n) => 
  DifferentiableArg (BackpropDiff a (BoxedVec n b)) (BoxedVec n (BackpropDiff a b)) where
    startDiff :: BackpropDiff a (BoxedVec n b) -> BoxedVec n (BackpropDiff a b) -- DiffArg a (BoxedVec n b) (BoxedVec n c)
    startDiff = boxedVecArg startDiff

boxedVecVal :: (a -> b) -> BoxedVec n a -> BoxedVec n b
boxedVecVal = fmap

instance (DifferentialVal a b) =>
  DifferentialVal (BoxedVec n a) (BoxedVec n b) where
    autoVal :: BoxedVec n a -> BoxedVec n b
    autoVal = boxedVecVal autoVal


-- Stream

packStream :: Additive (CT b) => BackpropDiff a (Stream b) -> Stream (BackpropDiff a b)
packStream (MkBackpropDiff x bpc) = DS.Cons
    (MkBackpropDiff x_head bpc_head)
    (packStream (MkBackpropDiff x_tail bpc_tail))
  where
    x_head = DS.head x
    x_tail = DS.tail x
    bpc_head = bpc . DF.unit
    bpc_tail = bpc . DF.bJoin zero

streamArg :: Additive (CT b) => 
  (BackpropDiff a b -> c) -> BackpropDiff a (Stream b) -> Stream c
streamArg f = fmap f . packStream

instance (DifferentiableArg a b, Additive (CT b)) => 
  DifferentiableArg (BackpropDiff a (Stream b)) (Stream (BackpropDiff a b)) where
    startDiff :: BackpropDiff a (Stream b) -> Stream (BackpropDiff a b)
    startDiff = streamArg startDiff

streamVal :: (a -> b) -> Stream a -> Stream b
streamVal = fmap

instance (DifferentialVal a b) =>
  DifferentialVal (Stream a) (Stream b) where
    autoVal :: Stream a -> Stream b
    autoVal = streamVal autoVal