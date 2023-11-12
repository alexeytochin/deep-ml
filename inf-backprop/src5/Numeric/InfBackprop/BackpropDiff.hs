{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-export-lists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}


module Numeric.InfBackprop.BackpropDiff where


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
import Control.Applicative ((<$>))
import Data.Type.Equality (type (~))
import Prelude.Tools (cross, cross3)
import GHC.Generics (Generic)
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
import NumHask.Extra (vecBasis)
import Prelude.Tools (curry3, uncurry3)
import qualified Data.FiniteList as DF
import Control.ExtendableMap (ExtandableMap, extendMap)
import Data.Coerce (coerce)
import Data.Function (on)
--import qualified Data.Vector.Generic.Mutable as M
--import qualified Data.Vector.Generic as G
--import qualified Data.Vector.Primitive as P

--data BackpropDiff t a = forall h. MkBackpropDiff {value :: a, backpropCache :: h -> CT a -> CT t, cache :: h}
data BackpropDiff t a = MkBackpropDiff {value :: a, backprop :: CT a -> CT t} deriving (Generic)

--backprop :: BackpropDiff t a -> CT a -> CT t
--backprop (MkBackpropDiff _ bpc h) = bpc h

initDiff :: a -> BackpropDiff a a
--initDiff x = MkBackpropDiff x (const id) ()
initDiff x = MkBackpropDiff x id

call :: (BackpropDiff a a -> BackpropDiff a b) -> a -> b
call f = value . f . initDiff

type instance Tangent (BackpropDiff a b) = BackpropDiff a (Tangent b)
type instance Dual (BackpropDiff a b) = BackpropDiff a (Dual b)

derivativeOp :: (BackpropDiff a a -> BackpropDiff a b) ->
  a ->
  CT b ->
  CT a
derivativeOp f = backprop . f . initDiff

simpleDerivative :: forall a b. (Multiplicative (CT b)) =>
  (BackpropDiff a a -> BackpropDiff a b) ->
  a ->
  CT a
simpleDerivative f x = derivativeOp f x one




-- Common
class DerivativeWith a where
  initBackProp :: a -> CT a

instance (DerivativeWith a, Additive (CT t)) =>
  DerivativeWith (BackpropDiff t a) where
--    initBackProp (MkBackpropDiff x _ _) = constDiff (initBackProp x)
    initBackProp (MkBackpropDiff x _) = constDiff (initBackProp x)

instance DerivativeWith Float where
  initBackProp = const 1

instance DerivativeWith SimpleExpr where
  initBackProp = const one

instance DerivativeWith a => DerivativeWith (Traced a) where
  --initBackProp = extendMap initBackProp
  initBackProp (MkTraced x) = MkTraced $ initBackProp x

--type instance Val a (b0, b1, b2) = (Val a b0, Val a b1, Val a b2)
--type instance Val a (Maybe b) = Maybe (Val a b)
--type instance Val a (Stream b) = Stream (Val a b)
-- type instance Val t SimpleExpr = SimpleExpr


type Differentiable a b = a -> b -> DiffOutput a b

class HasDerivative a b | b -> a where
  type DiffOutput a b :: Type
  diff :: Differentiable a b

instance DerivativeWith b =>
  HasDerivative a (BackpropDiff a b) where
    type DiffOutput a (BackpropDiff a b) = CT a
    diff :: Differentiable a (BackpropDiff a b)
--    diff _ (MkBackpropDiff y bp h) = bp h $ initBackProp y
    diff _ (MkBackpropDiff y bp) = bp $ initBackProp y

derivative :: HasDerivative a b =>
  (BackpropDiff a a -> b) ->
  a ->
  DiffOutput a b
derivative f x = diff x (f (initDiff x))
--derivative f x = simpleDerivative ((diff x) f)


customArgDerivative :: HasDerivative a b =>
  (BackpropDiff a a -> c) ->
  (c -> b) ->
  a ->
  DiffOutput a b
customArgDerivative arg f = derivative (f . arg)


class HasGradient__ t a c | t c -> a where
  anyArg__ :: BackpropDiff t a -> c

instance HasGradient__ t a (BackpropDiff t a) where
  anyArg__ = id

instance (
    HasGradient__ t a0 c0,
    HasGradient__ t a1 c1,
    Additive (CT a0),
    Additive (CT a1)
  ) =>
  HasGradient__ t (a0, a1) (c0, c1) where
    anyArg__ =  cross anyArg__ anyArg__ . tupleArg

instance (
    HasGradient__ t a0 c0,
    HasGradient__ t a1 c1,
    HasGradient__ t a2 c2,
    Additive (CT a0),
    Additive (CT a1),
    Additive (CT a2)
  ) =>
  HasGradient__ t (a0, a1, a2) (c0, c1, c2) where
    anyArg__ =  cross3 anyArg__ anyArg__ anyArg__ . tripleArg

--tupleArg :: (Additive (CT a0), Additive (CT a1)) =>
--  BackpropDiff t (a0, a1) -> (BackpropDiff t a0, BackpropDiff t a1)

instance (
    HasGradient__ t a c, -- (BoxedVec n a) a (BackpropDiff (BoxedVec n a) a)
    Additive (CT a),
    KnownNat n
  ) =>
  HasGradient__ t (BoxedVec n a) (BoxedVec n c) where
    anyArg__ =  fmap anyArg__ . boxedVecArg

instance (
    HasGradient__ t a c,
    Additive (CT a)
  ) =>
  HasGradient__ t (DS.Stream a) (DS.Stream c) where
    anyArg__ =  fmap anyArg__ . streamArg

--instance (
--    HasGradient__ t a c,
--    Additive (CT a)
--  ) =>
--  HasGradient__ t (BoundedStream a) (BoundedStream c) where
--    anyArg__ =  fmap anyArg__ . boundedStreamArg

instance (
    HasGradient__ t a c
  ) =>
  HasGradient__ t (Maybe a) (Maybe c) where
    anyArg__ =  fmap anyArg__ . maybeArg





class HasGradient_ a c | c -> a where
  anyArg_ :: BackpropDiff a a -> c

instance HasGradient_ a (BackpropDiff a a) where
  anyArg_ = id

--instance HasGradient_ a =>  HasGradient_ (Maybe a) (Maybe (BackpropDiff t a)) where
--  anyArg_ = maybeArg

temp3 :: (
    Additive (CT a),
    KnownNat n
  ) => BackpropDiff t (BoxedVec n (DS.Stream a)) -> BoxedVec n (DS.Stream (BackpropDiff t a))
temp3 = fmap streamArg . boxedVecArg

--instance HasGradient_ t a =>
--  HasGradient_ (BoxedVec n a) (BoxedVec n (BackpropDiff t a)) where
--    anyArg_ =  fmap anyArg_ . boxedVecArg




class HasGradient a c | c -> a where
  anyArg :: BackpropDiff a a -> c

instance HasGradient a (BackpropDiff a a) where
  anyArg = id

gradient :: forall b c a. (HasGradient__ a a c, Multiplicative (CT b)) =>
--  Proxy b ->
  (c -> BackpropDiff a b) ->
  a ->
  CT a
gradient f = simpleDerivative (f . anyArg__)

customGradient_ :: (HasGradient a c, Multiplicative (CT b)) =>
  (b -> BackpropDiff a b) ->
  (c -> b) ->
  a ->
  CT a
customGradient_ value_ f = simpleDerivative (value_ . f . anyArg)

-- tupleGradient :: (HasGradient (a0, a1) (BackpropDiff (a0, a1) a0, BackpropDiff (a0, a1) a1)) =>
--   ((BackpropDiff (a0, a1) a0, BackpropDiff (a0, a1) a1) -> BackpropDiff (a0, a1) b) ->
--   (a0, a1) ->
--   CT (a0, a1)
-- tupleGradient = customGradient tupleValue






-- autoDiff :: (HasDerivative a b, HasGradient a c) =>
--   (c -> b) ->
--   a ->
--   DiffOutput a b
-- autoDiff f = derivative (f . anyArg)

--Tuples
instance (HasDerivative a b0, HasDerivative a b1) =>
  HasDerivative a (b0, b1) where
    type DiffOutput a (b0, b1) = (DiffOutput a b0, DiffOutput a b1)
    diff :: Differentiable a (b0, b1)
    diff x (y0, y1) = (diff x y0, diff x y1)

tupleArg :: (Additive (CT a0), Additive (CT a1)) =>
  BackpropDiff t (a0, a1) -> (BackpropDiff t a0, BackpropDiff t a1)
--tupleArg (MkBackpropDiff (x0, x1) bpc h) = (
--    MkBackpropDiff x0 (\h_ cy -> bpc h_ (cy, zero)) h,
--    MkBackpropDiff x1 (\h_ cy -> bpc h_ (zero, cy)) h
--  )
tupleArg (MkBackpropDiff (x0, x1) bpc) = (
    MkBackpropDiff x0 (\cy -> bpc (cy, zero)),
    MkBackpropDiff x1 (\cy -> bpc (zero, cy))
  )

--instance (Additive (CT a0), Additive (CT a1)) =>
--  HasGradient (a0, a1) (BackpropDiff (a0, a1) a0, BackpropDiff (a0, a1) a1) where
--    anyArg = tupleArg


--instance (Additive (CT a0), Additive (CT a1)) =>
--  HasGradient_ (a0, a1) (BackpropDiff (a0, a1) a0, BackpropDiff (a0, a1) a1) where
--    anyArg = tupleArg


tupleDerivative :: (HasDerivative (a0, a1) b, Additive (CT a0), Additive (CT a1)) =>
  ((BackpropDiff (a0, a1) a0, BackpropDiff (a0, a1) a1) -> b) ->
  (a0, a1) ->
  DiffOutput (a0, a1) b
tupleDerivative = customArgDerivative tupleArg

tupleDerivativeOverX :: (HasDerivative a0 b, Additive (CT a0)) =>
  ((BackpropDiff a0 a0, BackpropDiff a0 a1) -> b) ->
  (a0, a1) ->
  DiffOutput a0 b
tupleDerivativeOverX f (x0, x1) = derivative (\x -> f (x, constDiff x1)) x0

tupleDerivativeOverY :: (HasDerivative a1 b, Additive (CT a1)) =>
  ((BackpropDiff a1 a0, BackpropDiff a1 a1) -> b) ->
  (a0, a1) ->
  DiffOutput a1 b
tupleDerivativeOverY f (x0, x1) = derivative (\x -> f (constDiff x0, x)) x1

twoArgsDerivative :: (HasDerivative (a0, a1) b, Additive (CT a0), Additive (CT a1)) =>
  (BackpropDiff (a0, a1) a0 -> BackpropDiff (a0, a1) a1 -> b) ->
  a0 ->
  a1 ->
  DiffOutput (a0, a1) b
twoArgsDerivative f = curry (derivative $ uncurry f . tupleArg)

derivative2ArgsOverX :: (HasDerivative a0 b, Additive (CT a0)) =>
  (BackpropDiff a0 a0 -> BackpropDiff a0 a1 -> b) ->
  a0 ->
  a1 ->
  DiffOutput a0 b
derivative2ArgsOverX f x0 x1 = derivative (\x -> f x (constDiff x1)) x0     -- diff (f identity (constDFunc x1)) x0

derivative2ArgsOverY :: (HasDerivative a1 b, Additive (CT a1)) =>
  (BackpropDiff a1 a0 -> BackpropDiff a1 a1 -> b) ->
  a0 ->
  a1 ->
  DiffOutput a1 b
derivative2ArgsOverY f x0 = derivative (f (constDiff x0))

-- Triples
tripleArg :: (Additive (CT a0), Additive (CT a1), Additive (CT a2)) =>
  BackpropDiff t (a0, a1, a2) -> (BackpropDiff t a0, BackpropDiff t a1, BackpropDiff t a2)
--tripleArg (MkBackpropDiff (x0, x1, x2) bpc h) = (
--    MkBackpropDiff x0 (\h_ cy -> bpc h_ (cy, zero, zero)) h,
--    MkBackpropDiff x1 (\h_ cy -> bpc h_ (zero, cy, zero)) h,
--    MkBackpropDiff x2 (\h_ cy -> bpc h_ (zero, zero, cy)) h
--  )
tripleArg (MkBackpropDiff (x0, x1, x2) bpc) = (
    MkBackpropDiff x0 (\cy -> bpc (cy, zero, zero)),
    MkBackpropDiff x1 (\cy -> bpc (zero, cy, zero)),
    MkBackpropDiff x2 (\cy -> bpc (zero, zero, cy))
  )

tripleDerivative :: (HasDerivative (a0, a1, a2) b, Additive (CT a0), Additive (CT a1), Additive (CT a2)) =>
  ((BackpropDiff (a0, a1, a2) a0, BackpropDiff (a0, a1, a2) a1, BackpropDiff (a0, a1, a2) a2) -> b) ->
  (a0, a1, a2) ->
  DiffOutput (a0, a1, a2) b
tripleDerivative = customArgDerivative tripleArg

instance (Additive (CT a0), Additive (CT a1), Additive (CT a2)) =>
  HasGradient (a0, a1, a2) (BackpropDiff (a0, a1, a2) a0, BackpropDiff (a0, a1, a2) a1, BackpropDiff (a0, a1, a2) a2) where
    anyArg = tripleArg

threeArgsDerivative :: (HasDerivative (a0, a1, a2) b, Additive (CT a0), Additive (CT a1), Additive (CT a2)) =>
  (BackpropDiff (a0, a1, a2) a0 -> BackpropDiff (a0, a1, a2) a1 -> BackpropDiff (a0, a1, a2) a2 -> b) ->
  a0 ->
  a1 ->
  a2 ->
  DiffOutput (a0, a1, a2) b
threeArgsDerivative f = curry3 (derivative $ uncurry3 f . tripleArg)

derivative3ArgsOverX :: (HasDerivative a0 b, Additive (CT a0)) =>
  (BackpropDiff a0 a0 -> BackpropDiff a0 a1 -> BackpropDiff a0 a2 -> b) ->
  a0 ->
  a1 ->
  a2 ->
  DiffOutput a0 b
derivative3ArgsOverX f x0 x1 x2 = derivative (\x -> f x (constDiff x1) (constDiff x2)) x0 -- diff (f identity (constDFunc x1) (constDFunc x2)) x0

derivative3ArgsOverY :: (HasDerivative a1 b, Additive (CT a1)) =>
  (BackpropDiff a1 a0 -> BackpropDiff a1 a1 -> BackpropDiff a1 a2 -> b) ->
  a0 ->
  a1 ->
  a2 ->
  DiffOutput a1 b
derivative3ArgsOverY f x0 x1 x2 = derivative (\x -> f (constDiff x0) x (constDiff x2)) x1 -- diff (f (constDFunc x0) identity (constDFunc x2)) x1

derivative3ArgsOverZ :: (HasDerivative a2 b, Additive (CT a2)) =>
  (BackpropDiff a2 a0 -> BackpropDiff a2 a1 -> BackpropDiff a2 a2 -> b) ->
  a0 ->
  a1 ->
  a2 ->
  DiffOutput a2 b
derivative3ArgsOverZ f x0 x1 = derivative (f (constDiff x0) (constDiff x1)) -- diff (f (constDFunc x0) (constDFunc x1) identity)


-- LongVec
-- type LongVec (n :: Nat) a = DVGS.Vector DV.Vector n a
--instance (HasDerivative a b) =>
--  HasDerivative a (LongVec n b) where
--    type DiffOutput a (LongVec n b) = LongVec n (DiffOutput a b)
--    diff :: Differentiable a (LongVec n b)
--    diff = fmap . diff
instance (Functor v, HasDerivative a b) =>
  HasDerivative a (DVGS.Vector v n b) where
    type DiffOutput a (DVGS.Vector v n b) = DVGS.Vector v n (DiffOutput a b)
    diff :: Differentiable a (DVGS.Vector v n b)
    diff = fmap . diff

--instance (Prim a, Prim t) => DVGM.MVector DVP.MVector (BackpropDiff t a) where
--    basicLength = undefined -- DVGM.basicLength . coerce
--    basicUnsafeSlice = undefined -- DVGM.basicUnsafeSlice
--    basicOverlaps = undefined -- DVGM.basicOverlaps `on` coerce
--    basicUnsafeNew = undefined -- DVGM.basicUnsafeNew
--    basicInitialize = undefined -- DVGM.basicInitialize
--    basicUnsafeReplicate n = undefined -- coerce . DVGM.basicUnsafeReplicate n
--    basicUnsafeRead v i = undefined -- coerce <$> basicUnsafeRead (coerce v) i
--    basicUnsafeWrite v i x = undefined -- basicUnsafeWrite (coerce v) i (coerce x)
--    basicClear = undefined -- basicClear . coerce
--    basicSet v x = undefined -- basicSet (coerce v) (coerce x)
--    basicUnsafeCopy dst src = undefined -- basicUnsafeCopy (coerce dst) (coerce src)
--    basicUnsafeMove dst src = undefined -- basicUnsafeMove (coerce dst) (coerce src)
--    basicUnsafeGrow v n = undefined -- coerce <$> basicUnsafeGrow (coerce v) n
--
--instance (Prim a, Prim t) => DVG.Vector DVP.Vector (BackpropDiff t a) where
--    basicUnsafeFreeze = undefined -- coerce . basicUnsafeFreeze . coerce
--    basicUnsafeThaw = undefined -- coerce . basicUnsafeThaw . coerce
--    basicLength = undefined -- basicLength . coerce
--    basicUnsafeSlice i n = undefined -- coerce . basicUnsafeSlice i n . coerce
--    basicUnsafeIndexM v i = undefined -- coerce <$> basicUnsafeIndexM (coerce v) i
--    basicUnsafeCopy dst src = undefined -- basicUnsafeCopy (coerce dst) (coerce src)
--    elemseq v x y = undefined -- elemseq (coerce v) (coerce x) y


--instance DVGBM.MVector DVP.MVector a => DVGBM.MVector DVP.MVector (BackpropDiff t a) where
--    basicLength = basicLength
--    basicUnsafeSlice = DVGBM.basicUnsafeSlice
--    basicOverlaps = DVGBM.basicOverlaps
--    basicUnsafeNew = DVGBM.basicUnsafeNew
--    basicInitialize = DVGBM.basicInitialize
--    basicUnsafeReplicate = DVGBM.basicUnsafeReplicate
--    basicUnsafeRead = DVGBM.basicUnsafeRead
--    basicUnsafeWrite = DVGBM.basicUnsafeWrite
--    basicClear = DVGBM.basicClear
--    basicSet = DVGBM.basicSet
--    basicUnsafeCopy = DVGBM.basicUnsafeCopy
--    basicUnsafeMove = DVGBM.basicUnsafeMove
--    basicUnsafeGrow = DVGBM.basicUnsafeGrow

--x = DVGB.basicUnsafeFreeze

-- deriving instance (Prim a, Prim t) => Prim (BackpropDiff t a)

--instance DVG.Vector DVP.Vector a => -- DVG.Vector v a =>
--  DVG.Vector DVP.Vector (BackpropDiff t a) where
--    basicUnsafeFreeze = basicUnsafeFreeze
--    basicUnsafeThaw = basicUnsafeThaw
--    basicLength = basicLength
--    basicUnsafeSlice = basicUnsafeSlice
--    basicUnsafeIndexM = basicUnsafeIndexM

boxedVecArg :: forall t a n. (
    Additive (CT a),
    KnownNat n
  ) =>
  BackpropDiff t (BoxedVec n a) -> BoxedVec n (BackpropDiff t a)
boxedVecArg (MkBackpropDiff vec bpc) = DVGS.generate $ \k ->
  MkBackpropDiff (DVGS.index vec k) (bpc . vecBasis k zero)

--applicativeArg :: forall t f f' a. (Functor f, Applicative f', CT (f a) ~ f' (CT a)) => 
--  BackpropDiff t (f a) -> f (BackpropDiff t a)
--applicativeArg (MkBackpropDiff x bp) = fmap (\a_ -> MkBackpropDiff a_ (bp . pure)) x


-- BackpropDiff t (DS.Stream a) -> DS.Stream (BackpropDiff t a)




--class HasGradient a c | c -> a where
--  anyArg :: BackpropDiff a a -> c
--
--instance HasGradient a (BackpropDiff a a) where
--  anyArg = id

--  HasGradient
--    (BoxedVec n (BoxedVec m a))
--    (BoxedVec n (BoxedVec m (BackpropDiff (BoxedVec n (BoxedVec m a)) a))) where

--instance (
--    KnownNat n,
--    Additive (CT a),
--    HasGradient a b -- HasGradient (DS.Stream a) (DS.Stream (BackpropDiff (DS.Stream a) a))
--  ) =>
--  HasGradient (BoxedVec n a) (BoxedVec n (BackpropDiff (BoxedVec n a) a)) where
--    anyArg = boxedVecArg

instance (
    KnownNat n,
    Additive (CT a)
    -- HasGradient a b
  ) =>
  HasGradient (BoxedVec n a) (BoxedVec n (BackpropDiff (BoxedVec n a) a)) where
    anyArg = boxedVecArg

boxedVecDerivative :: (KnownNat n, HasDerivative (BoxedVec n a) b, Additive (CT a)) =>
  (BoxedVec n (BackpropDiff (BoxedVec n a) a) -> b) ->
  BoxedVec n a ->
  DiffOutput (BoxedVec n a) b
boxedVecDerivative = customArgDerivative boxedVecArg


-- Stream
instance (HasDerivative a b) =>
  HasDerivative a (DS.Stream b) where
    type DiffOutput a (DS.Stream b) = DS.Stream (DiffOutput a b)
    diff :: Differentiable a (DS.Stream b)
    diff = fmap . diff

streamArg :: (Additive (CT a)) =>
  BackpropDiff t (DS.Stream a) -> DS.Stream (BackpropDiff t a)
--streamArg (MkBackpropDiff x bpc h) = DS.Cons
--    (MkBackpropDiff x_head bpc_head h)
--    (streamArg (MkBackpropDiff x_tail bpc_tail h))
--  where
--  x_head = DS.head x
--  x_tail = DS.tail x
--  bpc_head h_ = bpc h_ . DF.unit
--  bpc_tail h_ = bpc h_ . DF.bTail
streamArg (MkBackpropDiff x bpc) = DS.Cons
    (MkBackpropDiff x_head bpc_head)
    (streamArg (MkBackpropDiff x_tail bpc_tail))
  where
    x_head = DS.head x
    x_tail = DS.tail x
    bpc_head = bpc . DF.unit
    bpc_tail = bpc . DF.bJoin zero -- DF.bTail

instance (Additive (CT a)) =>
  HasGradient (DS.Stream a) (DS.Stream (BackpropDiff (DS.Stream a) a)) where
    anyArg = streamArg

streamDerivative :: (HasDerivative (DS.Stream a) b, Additive (CT a)) =>
  (DS.Stream (BackpropDiff (DS.Stream a) a) -> b) ->
  DS.Stream a ->
  DiffOutput (DS.Stream a) b
streamDerivative = customArgDerivative streamArg

-- Maybe
instance (HasDerivative a b) =>
  HasDerivative a (Maybe b) where
    type DiffOutput a (Maybe b) = Maybe (DiffOutput a b)
    diff :: Differentiable a (Maybe b)
    diff = fmap . diff

maybeArg :: BackpropDiff t (Maybe a) -> Maybe (BackpropDiff t a)
maybeArg (MkBackpropDiff maybeX bpc) = case maybeX of
  Just x -> Just (MkBackpropDiff x (bpc . Just))
  Nothing -> Nothing


applicativeArg :: forall t f f' a. (Functor f, Applicative f', CT (f a) ~ f' (CT a)) => 
  BackpropDiff t (f a) -> f (BackpropDiff t a)
applicativeArg (MkBackpropDiff x bp) = fmap (\a_ -> MkBackpropDiff a_ (bp . pure)) x

instance HasGradient (Maybe a) (Maybe (BackpropDiff (Maybe a) a)) where
  anyArg = maybeArg

maybeDerivative :: (HasDerivative (Maybe a) b) =>
  (Maybe (BackpropDiff (Maybe a) a) -> b) ->
  Maybe a ->
  DiffOutput (Maybe a) b
maybeDerivative = customArgDerivative maybeArg


--class HasGradient1 a  where
--  anyArg1 :: BackpropDiff t (f a) -> f (BackpropDiff t a)
--
--class HasGradient2 (f :: Type -> Type -> Type) where
--  anyArg2 :: BackpropDiff t (f a b) -> f (BackpropDiff t a) (BackpropDiff t b)

-- BackpropDiff t (BoxedVec n a) -> BoxedVec n (BackpropDiff t a)






-- Funcs
simpledFunc :: Multiplicative (CT a) =>
  (a -> a) -> (a -> CT a) -> BackpropDiff t a -> BackpropDiff t a
--simpledFunc f f' (MkBackpropDiff x bpc h) = MkBackpropDiff (f x) (\h_ cy -> bpc h_ $ f' x * cy) h
simpledFunc f f' (MkBackpropDiff x bpc) = MkBackpropDiff (f x) (\cy -> bpc $ f' x * cy)

constDiff :: Additive (CT t) => 
  a -> BackpropDiff t a
--constDiff x = MkBackpropDiff x (const zero) ()
constDiff x = MkBackpropDiff x (const zero)

constDFunc :: (Additive (CT a)) =>
  b -> BackpropDiff t a -> BackpropDiff t b
--constDFunc x (MkBackpropDiff _ bpc h) = MkBackpropDiff x (\h_ _ -> bpc h_ zero) h
constDFunc x (MkBackpropDiff _ bpc) = MkBackpropDiff x (\_ -> bpc zero)


tupleAdd :: Additive a => BackpropDiff t (a, a) -> BackpropDiff t a
--tupleAdd (MkBackpropDiff (x0, x1) bpc h) = MkBackpropDiff (x0 + x1) (\h_ cy -> bpc h_ (cy, cy)) h
tupleAdd (MkBackpropDiff (x0, x1) bpc) = MkBackpropDiff (x0 + x1) (\cy -> bpc (cy, cy))

tupleSub :: (Subtractive a, Subtractive (CT a)) => BackpropDiff t (a, a) -> BackpropDiff t a
--tupleSub (MkBackpropDiff (x0, x1) bpc h) = MkBackpropDiff (x0 - x1) (\h_ cy -> bpc h_ (cy, negate cy)) h
tupleSub (MkBackpropDiff (x0, x1) bpc) = MkBackpropDiff (x0 - x1) (\cy -> bpc (cy, negate cy))

tupleMult :: (Multiplicative a, a ~ CT a) => BackpropDiff t (a, a) -> BackpropDiff t a
--tupleMult (MkBackpropDiff (x0, x1) bpc h) = MkBackpropDiff (x0 * x1) (\h_ cy -> bpc h_ (cy * x1, x0 * cy)) h
tupleMult (MkBackpropDiff (x0, x1) bpc) = MkBackpropDiff (x0 * x1) (\cy -> bpc (cy * x1, x0 * cy))

tupleDiv :: (Divisive a, Subtractive a, a ~ CT a) => BackpropDiff t (a, a) -> BackpropDiff t a
--tupleDiv (MkBackpropDiff (x0, x1) bpc h) = MkBackpropDiff
--  (x0 / x1)
--  (\(r_, h_) cy -> bpc h_ (r_ * cy, negate $ x0 * r^2 * cy))
--  (r, h)
--  where
--    r = recip x1
tupleDiv (MkBackpropDiff (x0, x1) bpc) = MkBackpropDiff
  (x0 / x1)
  (\cy -> bpc (r * cy, negate $ x0 * r^2 * cy))
  where
    r = recip x1


twoArgsToTuple :: (Additive (CT t)) =>
  BackpropDiff t a -> BackpropDiff t b -> BackpropDiff t (a, b)
--twoArgsToTuple (MkBackpropDiff x0 bpc0 h0) (MkBackpropDiff x1 bpc1 h1) =
--  MkBackpropDiff (x0, x1) (\(h0_, h1_) (cy0, cy1) -> bpc0 h0_ cy0 + bpc1 h1_ cy1) (h0, h1)
twoArgsToTuple (MkBackpropDiff x0 bpc0) (MkBackpropDiff x1 bpc1) =
  MkBackpropDiff (x0, x1) (\(cy0, cy1) -> bpc0 cy0 + bpc1 cy1)

threeArgsToTriple :: (Additive (CT t)) =>
  BackpropDiff t a -> BackpropDiff t b -> BackpropDiff t c -> BackpropDiff t (a, b, c)
--threeArgsToTriple (MkBackpropDiff x0 bpc0 h0) (MkBackpropDiff x1 bpc1 h1) (MkBackpropDiff x2 bpc2 h2) =
--  MkBackpropDiff (x0, x1, x2) (\(h0_, h1_, h2_) (cy0, cy1, cy2) -> bpc0 h0_ cy0 + bpc1 h1_ cy1 + bpc2 h2_ cy2) (h0, h1, h2)
threeArgsToTriple (MkBackpropDiff x0 bpc0) (MkBackpropDiff x1 bpc1) (MkBackpropDiff x2 bpc2) =
  MkBackpropDiff (x0, x1, x2) (\(cy0, cy1, cy2) -> bpc0 cy0 + bpc1 cy1 + bpc2 cy2)



-- Distributive
instance (Additive (CT t), Additive a) =>
  Additive (BackpropDiff t a) where
    zero = constDiff zero
    -- (MkDiff x bpx hx) + (MkDiff y bpy hy) = MkDiff (x + y) (\(hx_, hy_) cz -> bpx hx_ cz + bpy hy_ cz) (hx, hy)
    x + y = tupleAdd $ twoArgsToTuple x y

instance (Additive (CT t), Subtractive a, Distributive (CT a), Subtractive (CT a)) =>
  Subtractive (BackpropDiff t a) where
    negate = simpledFunc negate (const $ negate one)
    -- (MkDiff x bpx hx) - (MkDiff y bpy hy) = MkDiff (x - y) (\(hx_, hy_) cz -> bpx hx_ cz + bpy hy_ (negate cz)) (hx, hy)
    x - y = tupleSub $ twoArgsToTuple x y

instance (Additive (CT t), Multiplicative a, a ~ CT a) =>
  Multiplicative (BackpropDiff t a) where
    one = constDiff one
    -- (MkDiff x bpx hx) * (MkDiff y bpy hy) = MkDiff (x * y) (\(hx_, hy_) cz -> bpx hx_ (cz * y) + bpy hy_ (x * cz)) (hx, hy)
    x * y = tupleMult $ twoArgsToTuple x y

#if MIN_VERSION_numhask(0,11,0)
#else
instance (Additive ca, Distributive cb, b ~ cb) =>
  Distributive (LensD ca a cb b)
#endif

-- Divisible
--square :: Multiplicative a => a -> a
--square a = a * a

--squareDLens :: (Distributive a, CT a ~ a) =>
--  DFunc a a
--squareDLens = scalarDFunc square (two *)
--
--recipDFunc :: (Divisive a, Subtractive a, CT a ~ a) =>
--  DFunc a a
--recipDFunc = scalarDFunc recip (\x -> negate $ recip x^2)

instance (Additive (CT t), Divisive a, Subtractive a, CT a ~ a) =>
  Divisive (BackpropDiff t a) where
--    recip (MkBackpropDiff x bpc h) = MkBackpropDiff
--      r
--      (\(r_, h_) cy -> bpc h_ $ negate $ r_^2 * cy )
--      (r, h)
--      where r = recip x
--    x / y = tupleDiv $ twoArgsToTuple x y
    recip (MkBackpropDiff x bpc) = MkBackpropDiff
      r
      (\cy -> bpc $ negate $ r^2 * cy )
      where r = recip x
    x / y = tupleDiv $ twoArgsToTuple x y


instance (FromIntegral a b, Additive (CT t)) =>
  FromIntegral (BackpropDiff t a) b where
    fromIntegral :: b -> BackpropDiff t a
    fromIntegral x = constDiff (fromIntegral x)

-- Pow
instance (Additive (CT t), Subtractive a, IntegralPower p a, FromIntegral a p, CT a ~ a) =>
  IntegralPower p (BackpropDiff t a) where
    integralPow n = simpledFunc (integralPow n) (\x -> fromIntegral n * integralPow (n - one) x)


-- Field
#if MIN_VERSION_numhask(0,11,0)
#else
instance (Additive ct, Field a, a ~ CT a) =>
  Field (BackpropDiff t a)
#endif

--sqrtC :: (ExpField a, a ~ CT a) => Diff t a -> Diff t a
--sqrtC (MkDiff x bp) = MkDiff y (bp . recip . (two *) . (y *)) where
--  y = sqrt x

instance (ExpField a, Additive (CT t), CT a ~ a) =>
  ExpField (BackpropDiff t a) where
    exp = simpledFunc exp exp
    log = simpledFunc log recip
--    (MkBackpropDiff x bpx hx) ** (MkBackpropDiff n bpn hn) =
--      MkBackpropDiff y (\(y_, hx_, hn_) cy -> bpx hx_ (bp1 * cy) + bpn hn_ (bp2 y_ * cy)) (y, hx, hn) where
--        bp1 = n * (x ** (n - one))
--        bp2 y_= log x * y_
--        y = x ** n
    (MkBackpropDiff x bpx) ** (MkBackpropDiff n bpn) =
      MkBackpropDiff y (\cy -> bpx (bp1 * cy) + bpn (bp2 y * cy)) where
        bp1 = n * (x ** (n - one))
        bp2 y_= log x * y_
        y = x ** n
    sqrt (MkBackpropDiff x bp) = MkBackpropDiff y (\cy -> bp $ recip $ two * y * cy) where
      y = sqrt x

instance (Additive (CT t), ExpField a, TrigField a, a ~ CT a) =>
  TrigField (BackpropDiff t a) where
    sin = simpledFunc sin cos
    cos = simpledFunc cos (negate . sin)
    asin = undefined
    acos = undefined
    atan = undefined
    atan2 = undefined
    sinh = undefined
    cosh = undefined
    asinh = undefined
    acosh = undefined
    atanh = undefined
    pi = constDiff pi


class ToConst a b where
  stopDiff :: a -> b

instance ToConst a a where
  stopDiff = id

instance (Additive (CT t), ToConst a b) =>
  ToConst a (BackpropDiff t b) where
    stopDiff = constDiff . stopDiff

_ = stopDiff (42 :: Float) :: Float

temp1 :: forall t. Additive (CT t) =>
  BackpropDiff t Float
temp1 = stopDiff (42 :: Float)

temp2 :: forall t1 t2. (Additive (CT t1), Additive (CT t2)) =>
  BackpropDiff t2 (BackpropDiff t1 Float)
temp2 = stopDiff (42 :: Float)