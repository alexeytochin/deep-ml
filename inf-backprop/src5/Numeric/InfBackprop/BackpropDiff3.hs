{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveGeneric #-}

module Numeric.InfBackprop.BackpropDiff3 (
    BackpropDiff(MkBackpropDiff, value, backprop),
    initDiff,
    call,
    constDiff,
    derivativeOp,
    fieldDerivative,
    simpleDerivative,
    DifferentiableArg,
    -- GetDiffArgInput,
    -- GetDiffArgOutput,
    DifferentiableArgScalar,
    polymorphicDiff,
    polymorphicArg,
    customDerivative,
    customArgDerivative,
    derivative,
    gradient,
    tupleArg,
    tupleVal,
    -- tupleDerivative,
    -- twoArgsDerivative,
    -- derivative2ArgsOverX,
    -- derivative2ArgsOverY,
    boxedVecVal,
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
import NumHask.Extra (vecBasis)
import Prelude.Tools (curry3, uncurry3)
import qualified Data.FiniteList as DF
import Control.ExtendableMap (ExtandableMap, extendMap)
import Data.Coerce (coerce)
import Data.Function (on)
import Data.Stream (Stream)
import Data.Tuple.Extra ((***))
import Data.FiniteList (BoundedStream)
-- import Numeric.InfBackprop.BackpropDiff (HasDerivative(DiffOutput))


data BackpropDiff t a = 
    MkBackpropDiff {value :: a, backprop :: CT a -> CT t} deriving (Generic)

initDiff :: a -> BackpropDiff a a
initDiff x = MkBackpropDiff x id

call :: (BackpropDiff a a -> BackpropDiff a b) -> a -> b
call f = value . f . initDiff

withField :: BackpropDiff a b -> (b -> CT b) -> CT a
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
-- fieldDerivative f x = withField (f (initDiff x))
fieldDerivative f = withField . f . initDiff

-- | Derivative scalar -> scalar
simpleDerivative :: forall a b. (Multiplicative (CT b)) =>
  (BackpropDiff a a -> BackpropDiff a b) ->
  a ->
  CT a
-- simpleDerivative f x = derivativeOp f x one
simpleDerivative f x = fieldDerivative f x (const one) 


-- class (Output a ~ b, Input b ~ a) => Getter a b where
--   type Input a :: Type
--   type Output b :: Type
--   to :: a -> b

class -- (GetDiffArgOutput a b ~ c, GetDiffArgInput c ~ BackpropDiff a b) => 
  DifferentiableArg a b c | a b -> c, c -> a b where
    -- type GetDiffArgOutput a b :: Type
    -- type GetDiffArgInput c :: Type
    -- type DiffInput 
    polymorphicArg :: BackpropDiff a b -> c

-- class -- (GetDiffArgOutput a b ~ c, GetDiffArgInput c ~ BackpropDiff a b) => 
--   DifferentiableArg a b | a -> b, b -> a where
--     -- type DiffInput 
--     polymorphicArg :: BackpropDiff a a -> b

type DifferentiableArgScalar a = DifferentiableArg a a (BackpropDiff a a)

-- instance DifferentiableArg a b (BackpropDiff a b) where
--   polymorphicArg = id

instance DifferentiableArg a Float (BackpropDiff a Float) where
  -- type GetDiffArgOutput a Float = BackpropDiff a Float
  -- type GetDiffArgInput (BackpropDiff a Float) = BackpropDiff a Float
  polymorphicArg = id

-- instance DifferentiableArg Float (BackpropDiff Float Float) where
--   polymorphicArg = id

instance DifferentiableArg a SimpleExpr (BackpropDiff a SimpleExpr) where
  -- type GetDiffArgOutput a SimpleExpr = BackpropDiff a SimpleExpr
  -- type GetDiffArgInput (BackpropDiff a SimpleExpr) = BackpropDiff a SimpleExpr
  polymorphicArg = id

-- instance DifferentiableArg SimpleExpr (BackpropDiff SimpleExpr SimpleExpr) where
--   polymorphicArg = id

-- class DifferentiableArg c where
--   type GetA c :: Type
--   type GetB c :: Type
--   polymorphicArg :: BackpropDiff (GetA c) (GetB c) -> c


-- type Differentiable a = a -> DiffOutput a


-- type instance DiffOutput a Float = CT a
-- type instance DiffOutput a SimpleExpr = CT a


class -- (b ~ DiffOutput a) =>  -- (GetDiffValOutput b c ~ a, GetDiffArgInput a ~ BackpropDiff b c) => 
  DifferentiableVal a b c | a -> b c, b c -> a where
  -- DifferentiableVal a where
    -- type DiffOutput b c :: Type
    -- type DiffOutput a :: Type
    -- type GetDiffValOutput b c :: Type
    -- type GetDiffValInput a :: Type
    -- polymorphicDiff :: a -> DiffOutput b c
    -- polymorphicDiff :: a -> DiffOutput a
    polymorphicDiff :: a -> BackpropDiff b c

class StartBackprop a where
  type DiffOutput t a :: Type
  -- startBackprop :: forall t. Proxy t -> Proxy a -> BackpropDiff t a -> DiffOutput t a
  startBackprop :: BackpropDiff t a -> DiffOutput t a

-- instance -- (Multiplicative (CT b)) =>
--   DifferentiableVal (BackpropDiff b Float) b Float where 
--     type DiffOutput b Float = CT b
--     polymorphicDiff :: BackpropDiff b Float -> DiffOutput b Float
--     polymorphicDiff bpd = backprop bpd 1

-- instance -- (Multiplicative (CT b)) =>
--   DifferentiableVal (BackpropDiff b SimpleExpr) b SimpleExpr where 
--     type DiffOutput b SimpleExpr = CT b
--     polymorphicDiff :: BackpropDiff b SimpleExpr -> DiffOutput b SimpleExpr
--     polymorphicDiff bpd = backprop bpd one

-- instance (Multiplicative (CT b)) =>
--   DifferentiableVal (BackpropDiff a b) where 
--     type DiffOutput (BackpropDiff a b) = CT a
--     polymorphicDiff :: BackpropDiff a b -> CT a
--     polymorphicDiff bpd = backprop bpd one

instance DifferentiableVal (BackpropDiff a Float) a Float where 
  polymorphicDiff :: BackpropDiff a Float -> BackpropDiff a Float
  polymorphicDiff = id

instance -- (Multiplicative (CT b)) =>
  StartBackprop Float where
    type DiffOutput a Float = CT a
    -- startBackprop :: forall a. Proxy a -> Proxy Float -> BackpropDiff a Float -> CT a
    -- startBackprop _ _ bpd = backprop bpd 1
    startBackprop :: BackpropDiff a Float -> CT a
    startBackprop bpd = backprop bpd 1

instance -- (Multiplicative (CT b)) =>
  StartBackprop SimpleExpr where
    type DiffOutput a SimpleExpr = CT a
    -- startBackprop :: forall a. Proxy a -> Proxy SimpleExpr -> BackpropDiff a SimpleExpr -> CT a
    -- startBackprop _ _ bpd = backprop bpd one
    startBackprop :: BackpropDiff a SimpleExpr -> CT a
    startBackprop bpd = backprop bpd one

-- instance 
--   DifferentiableVal (BackpropDiff t Float) where 
--     type DiffOutput (BackpropDiff t Float) = CT t
--     polymorphicDiff :: BackpropDiff t Float -> CT t
--     polymorphicDiff bpd = backprop bpd 1

-- instance 
--   DifferentiableVal (BackpropDiff t SimpleExpr) where 
--     type DiffOutput (BackpropDiff t SimpleExpr) = CT t
--     polymorphicDiff :: BackpropDiff t SimpleExpr -> CT t
--     polymorphicDiff bpd = backprop bpd one

-- instance Multiplicative (CT b) => 
--   DifferentiableVal (BackpropDiff a b) where 
--     polymorphicDiff = const one

-- class (DiffOutput a ~ b) => 
--   DifferentiableVal a b where
--     polymorphicDiff :: a -> b

customDerivative :: forall t a b c. StartBackprop c => -- forall b a c t. () =>
  (BackpropDiff t t -> a) ->
  -- (b -> BackpropDiff c t) ->
  (b -> BackpropDiff t c) ->
  (a -> b) -> 
  (t -> DiffOutput t c)
-- customDerivative arg val f =  startBackprop (Proxy @t) (Proxy @c). val . f . arg . initDiff
customDerivative arg val f =  startBackprop . val . f . arg . initDiff

-- derivative :: forall b a. (DifferentiableVal b (DiffOutput b)) =>
-- derivative :: forall b c a. (DifferentiableVal b c a) =>
--   (BackpropDiff a a -> b) -> a -> DiffOutput a c
-- derivative = customDerivative id polymorphicDiff

derivative :: forall c a b. (DifferentiableVal b a c, StartBackprop c) =>
  (BackpropDiff a a -> b) -> a -> DiffOutput a c
derivative = customDerivative id polymorphicDiff

gradient :: forall b a c. (DifferentiableArg a a c, StartBackprop b) =>
  (c -> BackpropDiff a b) -> a -> DiffOutput a b
-- gradient f = simpleDerivative (f . polymorphicArg)
gradient = customDerivative polymorphicArg id

-- customArgDerivative :: forall b a c t. (DifferentiableVal b c t) =>
--   (BackpropDiff t t -> a) ->
--   (a -> b) -> 
--   (t -> DiffOutput t c)
-- customArgDerivative arg = customDerivative arg polymorphicDiff

customArgDerivative :: (DifferentiableVal b t c, StartBackprop c) =>
  (BackpropDiff t t -> a) ->
  (a -> b) -> 
  (t -> DiffOutput t c)
customArgDerivative arg = customDerivative arg polymorphicDiff


-- gradient :: forall b c. (DifferentiableArg c, GetB c ~ b, Multiplicative (CT b)) =>
--   (c -> BackpropDiff (GetA c) (GetB c)) -> GetA c -> CT (GetA c)
-- gradient f = simpleDerivative (f . polymorphicArg)


-- type family DerivativeOutput a b :: Type

-- -- type instance DerivativeOutput a (BackpropDiff t b) = DerivativeOutput a (CT a)
-- type instance DerivativeOutput (BackpropDiff a a) (BackpropDiff a Float) = CT a
-- type instance DerivativeOutput (BackpropDiff a a) (BackpropDiff a SimpleExpr) = CT a


-- Tuple
-- type instance DerivativeOutput a (b0, b1) = (DerivativeOutput a b0, DerivativeOutput a b1)

tupleArg :: (Additive (CT a0), Additive (CT a1)) =>
  BackpropDiff t (a0, a1) -> (BackpropDiff t a0, BackpropDiff t a1)
tupleArg (MkBackpropDiff (x0, x1) bpc) = (
    MkBackpropDiff x0 (\cy -> bpc (cy, zero)),
    MkBackpropDiff x1 (\cy -> bpc (zero, cy))
  )

instance (
    DifferentiableArg a b0 c0, 
    DifferentiableArg a b1 c1, 
    Additive (CT b0), 
    Additive (CT b1)
  ) =>
  DifferentiableArg a (b0, b1) (c0, c1) where
    -- type GetDiffArgOutput a (b0, b1) = (GetDiffArgOutput a b0, GetDiffArgOutput a b1)
    -- type GetDiffArgInput (c0, c1) = BackpropDiff a (GetDiffArgInput c0, GetDiffArgInput c1)
    polymorphicArg = cross polymorphicArg polymorphicArg . tupleArg

-- instance (
--     DifferentiableArg a b0 c0, 
--     DifferentiableArg a b1 c1, 
--     Additive (CT b0), 
--     Additive (CT b1)
--   ) =>
--   DifferentiableArg a (b0, b1) (c0, c1) where
--     polymorphicArg :: BackpropDiff (a0, a1) (a0, a1) -> (BackpropDiff a0 a0, BackpropDiff a1 a1)
--     polymorphicArg = cross polymorphicArg polymorphicArg . tupleArg


-- instance (Additive (CT b0), Additive (CT b1)) =>
--   DifferentiableArg (BackpropDiff a b0, BackpropDiff a b1) where
--     polymorphicArg = tupleArg

-- type instance DiffOutput b (a0, a1) = (DiffOutput b a0, DiffOutput b a1)

tupleVal :: Additive (CT a) => 
  (BackpropDiff a b0, BackpropDiff a b1) -> BackpropDiff a (b0, b1)
tupleVal (MkBackpropDiff v0 bp0, MkBackpropDiff v1 bp1) = 
  MkBackpropDiff (v0, v1) (uncurry (+) . (bp0 *** bp1))

-- instance (DifferentiableVal a0 b c, DifferentiableVal a1 b c) =>
--   DifferentiableVal (BackpropDiff b0 c, BackpropDiff b1 c) (b0, b1) c where
--     -- type GetDiffValOutput (b0, b1) c = (BackpropDiff b0 c, BackpropDiff b1 c)
--     -- type GetDiffValInput (BackpropDiff b0 c, BackpropDiff b1 c) = BackpropDiff (b0, b1) c  
--     polymorphicDiff :: (a0, a1) -> (DiffOutput c a0, DiffOutput c a1)
--     polymorphicDiff = tupleVal (polymorphicDiff, polymorphicDiff)

    -- polymorphicDiff :: BoxedVec n a -> BackpropDiff b (BoxedVec n c)
    -- polymorphicDiff = boxedVecVal . fmap polymorphicDiff

instance (DifferentiableVal a0 b c0, DifferentiableVal a1 b c1, Additive (CT b)) =>
  DifferentiableVal (a0, a1) b (c0, c1) where
    -- type DiffOutput (a0, a1) = (DiffOutput a0, DiffOutput a1)
    polymorphicDiff :: (a0, a1) -> BackpropDiff b (c0, c1)
    polymorphicDiff = tupleVal . cross polymorphicDiff polymorphicDiff

-- class StartBackprop a where
--   type DiffOutput t a :: Type
--   stratBackprop :: BackpropDiff t a -> DiffOutput t a

instance forall a0 a1. (StartBackprop a0, StartBackprop a1, Additive (CT a0), Additive (CT a1)) =>
  StartBackprop (a0, a1) where
    type DiffOutput t (a0, a1) = (DiffOutput t a0, DiffOutput t a1)
    -- startBackprop :: forall t. Proxy t -> Proxy (a0, a1) -> BackpropDiff t (a0, a1) -> DiffOutput t (a0, a1) -- (DiffOutput t a0, DiffOutput t a1)
    -- startBackprop :: forall t. Proxy t -> Proxy (a0, a1) -> BackpropDiff t (a0, a1) -> DiffOutput t (a0, a1) -- (DiffOutput t a0, DiffOutput t a1)
    -- startBackprop _ _ (MkBackpropDiff (v0, v1) bp) = (
    --     startBackprop (Proxy @t) (Proxy @a0) $ MkBackpropDiff v0 (\cx -> bp (cx, zero)), 
    --     startBackprop (Proxy @t) (Proxy @a1) $ MkBackpropDiff v1 (\cx -> bp (zero, cx))
    --   ) 
    startBackprop :: forall t. BackpropDiff t (a0, a1) -> DiffOutput t (a0, a1) -- (DiffOutput t a0, DiffOutput t a1)
    startBackprop (MkBackpropDiff (v0, v1) bp) = (
        startBackprop @a0 @t $ MkBackpropDiff v0 (\cx -> bp (cx, zero)), 
        startBackprop @a1 @t $ MkBackpropDiff v1 (\cx -> bp (zero, cx))
      ) 






-- tupleDerivative :: (DifferentiableVal b, Additive (CT a0), Additive (CT a1)) =>
--   ((BackpropDiff (a0, a1) a0, BackpropDiff (a0, a1) a1) -> b) ->
--   (a0, a1) ->
--   DiffOutput b 
-- tupleDerivative = customArgDerivative tupleArg

-- twoArgsDerivative :: (DifferentiableVal b, Additive (CT a0), Additive (CT a1)) =>
--   (BackpropDiff (a0, a1) a0 -> BackpropDiff (a0, a1) a1 -> b) ->
--   a0 ->
--   a1 ->
--   DiffOutput b
-- twoArgsDerivative = curry . tupleDerivative . uncurry

-- derivative2ArgsOverX :: (DifferentiableVal b, Additive (CT a0)) =>
--   (BackpropDiff a0 a0 -> BackpropDiff a0 a1 -> b) ->
--   a0 ->
--   a1 ->
--   DiffOutput b
-- derivative2ArgsOverX f x0 x1 = derivative (\x -> f x (constDiff x1)) x0

-- derivative2ArgsOverY :: (DifferentiableVal b, Additive (CT a1)) =>
--   (BackpropDiff a1 a0 -> BackpropDiff a1 a1 -> b) ->
--   a0 ->
--   a1 ->
--   DiffOutput b
-- derivative2ArgsOverY f x0 = derivative (f (constDiff x0))




-- Triple

-- tripleArg :: (Additive (CT a0), Additive (CT a1), Additive (CT a2)) =>
--   BackpropDiff t (a0, a1, a2) -> (BackpropDiff t a0, BackpropDiff t a1, BackpropDiff t a2)
-- tripleArg (MkBackpropDiff (x0, x1, x2) bpc) = (
--     MkBackpropDiff x0 (\cy -> bpc (cy, zero, zero)),
--     MkBackpropDiff x1 (\cy -> bpc (zero, cy, zero)),
--     MkBackpropDiff x2 (\cy -> bpc (zero, zero, cy))
--   )

-- instance (Additive (CT b0), Additive (CT b1), Additive (CT b2)) =>
--   DifferentiableArg a (b0, b1, b2) (BackpropDiff a b0, BackpropDiff a b1, BackpropDiff a b2) where
--     type GetDiffArgOutput a (b0, b1, b2) = (BackpropDiff a b0, BackpropDiff a b1, BackpropDiff a b2)
--     type GetDiffArgInput (BackpropDiff a b0, BackpropDiff a b1, BackpropDiff a b2) = BackpropDiff a (b0, b1, b2)
--     polymorphicArg = tripleArg

-- type instance DiffOutput (a0, a1, a2) = (DiffOutput a0, DiffOutput a1, DiffOutput a2)

-- tripleVal :: 
--   (a0 -> DiffOutput a0, a1 -> DiffOutput a1, a2 -> DiffOutput a2) -> 
--   (a0, a1, a2) -> 
--   (DiffOutput a0, DiffOutput a1, DiffOutput a2)
-- tripleVal (f0, f1, f2) (y0, y1, y2) = (f0 y0, f1 y1, f2 y2)

-- instance (DifferentiableVal a0, DifferentiableVal a1, DifferentiableVal a2) =>
--   DifferentiableVal (a0, a1, a2) where
--     polymorphicDiff :: (a0, a1, a2) -> (DiffOutput a0, DiffOutput a1, DiffOutput a2)
--     polymorphicDiff = tripleVal (polymorphicDiff, polymorphicDiff, polymorphicDiff)


-- BoxedVec

boxedVecArg :: forall t a n. (
    Additive (CT a),
    KnownNat n
  ) =>
  BackpropDiff t (BoxedVec n a) -> BoxedVec n (BackpropDiff t a)
boxedVecArg (MkBackpropDiff vec bpc) = DVGS.generate $ \k ->
  MkBackpropDiff (DVGS.index vec k) (bpc . vecBasis k zero)

instance (DifferentiableArg a b c, Additive (CT b), KnownNat n) => -- , 
  DifferentiableArg a (BoxedVec n b) (BoxedVec n c) where
    -- type GetDiffArgOutput a (BoxedVec n b) = BoxedVec n (BackpropDiff a b)
    -- type GetDiffArgInput (BoxedVec n (BackpropDiff a b)) = BackpropDiff a (BoxedVec n b)
    polymorphicArg :: BackpropDiff a (BoxedVec n b) -> BoxedVec n c
    polymorphicArg = fmap polymorphicArg . boxedVecArg


-- boxedVecVal :: Additive (CT a) => BoxedVec n (BackpropDiff t a) -> BackpropDiff t (BoxedVec n a)
-- boxedVecVal v = MkBackpropDiff (fmap value v) (sum . fmap backprop v)
boxedVecVal = undefined


-- instance (DifferentiableVal a0 b c, DifferentiableVal a1 b c) =>
--   DifferentiableVal (BackpropDiff b0 c, BackpropDiff b1 c) (b0, b1) c where
--     type GetDiffValOutput (b0, b1) c = (BackpropDiff b0 c, BackpropDiff b1 c)
--     type GetDiffValInput (BackpropDiff b0 c, BackpropDiff b1 c) = BackpropDiff (b0, b1) c  
--     polymorphicDiff :: (a0, a1) -> (DiffOutput a0, DiffOutput a1)
--     polymorphicDiff = tupleVal (polymorphicDiff, polymorphicDiff)

instance (DifferentiableVal a b c) =>
  DifferentiableVal (BoxedVec n a) b (BoxedVec n c) where
    -- type DiffOutput (BoxedVec n a) = BoxedVec n (DiffOutput a)
    polymorphicDiff :: BoxedVec n a -> BackpropDiff b (BoxedVec n c)
    polymorphicDiff = boxedVecVal . fmap polymorphicDiff

-- Maybe
-- type instance DiffOutput b (Maybe a) = Maybe (DiffOutput b a)


-- Stream

streamArg :: forall t a. (
    Additive (CT a)
  ) =>
  BackpropDiff t (Stream a) -> Stream (BackpropDiff t a)
streamArg (MkBackpropDiff x bpc) = DS.Cons
    (MkBackpropDiff x_head bpc_head)
    (streamArg (MkBackpropDiff x_tail bpc_tail))
  where
    x_head = DS.head x
    x_tail = DS.tail x
    bpc_head = bpc . DF.unit
    bpc_tail = bpc . DF.bJoin zero

instance (Additive (CT b)) =>
  DifferentiableArg a (Stream b) (Stream (BackpropDiff a b)) where
    -- type GetDiffArgOutput a (Stream b) = Stream (BackpropDiff a b)
    -- type GetDiffArgInput (Stream (BackpropDiff a b)) = BackpropDiff a (Stream b)
    polymorphicArg = streamArg

-- type instance DiffOutput b (Stream a) = Stream (DiffOutput b a)

-- streamVal ::
--   (a -> DiffOutput b c) -> 
--   Stream a -> 
--   Stream (DiffOutput b c)
-- streamVal = fmap

streamVal :: Stream (BackpropDiff t a) -> BackpropDiff t (Stream a)
streamVal = undefined

-- instance -- (Multiplicative (CT b)) =>
--   DifferentiableVal (BackpropDiff b Float) b Float where 
--     type DiffOutput b Float = CT b
--     polymorphicDiff :: BackpropDiff b Float -> DiffOutput b Float
--     polymorphicDiff bpd = backprop bpd 1

-- class -- (GetDiffValOutput b c ~ a, GetDiffArgInput a ~ BackpropDiff b c) => 
--   DifferentiableVal a b c | a -> b c, b c -> a where
--     type DiffOutput b c :: Type
--     polymorphicDiff :: a -> DiffOutput b c

-- instance (DifferentiableVal a b c) =>
--   DifferentiableVal (Stream a) b c where
--     -- type GetDiffValOutput (BoxedVec n b) c = BoxedVec n (DiffOutput a c)
--     -- type GetDiffValInput (BackpropDiff b0 c, BackpropDiff b1 c) = BackpropDiff (b0, b1) c  
--     type DiffOutput b (Stream a) = Stream (DiffOutput b a)
--     polymorphicDiff :: Stream a -> Stream (DiffOutput b a)
--     polymorphicDiff = boxedVecVal polymorphicDiff

instance (DifferentiableVal a b c) =>
  DifferentiableVal (Stream a) b (Stream c) where
    polymorphicDiff :: Stream a -> BackpropDiff b (Stream c)
    polymorphicDiff = streamVal . fmap polymorphicDiff

instance () =>
  StartBackprop (Stream a) where
    type DiffOutput t (Stream a) = BoundedStream (DiffOutput t a)
    startBackprop = undefined
