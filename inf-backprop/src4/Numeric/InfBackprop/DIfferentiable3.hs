{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}

module Numeric.InfBackprop.DIfferentiable3 where

import Data.Type.Equality (type (~))
--import GHC.Base
--    ( flip,
--      Functor,
--      Maybe,
--      Type,
--      (==),
--      fmap,
--      Type,
--      const
--      )
import Data.Vector.Fixed.Cont (Peano)
import GHC.Natural (Natural)
import Data.Finite (Finite)
import GHC.Base (Type, const, undefined, id, (.), ($), (==), fmap, Maybe, Functor, flip)
import Numeric.InfBackprop.DFunc2 (DFunc(MkDFunc), identity, pullback, derivativeOp, constDFunc)
import NumHask (Multiplicative, one, Additive, zero)
import Numeric.InfBackprop.Tangent (CT, LongVec)
import GHC.TypeNats (KnownNat)
import Prelude.Tools (cross, fork, fork3)
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector as DV
import qualified Data.Vector.Fixed.Boxed as DVFB
import qualified Data.Vector.Fixed as DVF
import Data.Tuple (curry, uncurry)
import Data.Proxy (Proxy(Proxy))
import Debug.SimpleExpr (SimpleExpr)
import Data.List.NonEmpty (iterate)
import NumHask (Distributive)
import Data.Stream (Stream)

type family Val t a :: Type
type instance Val t (DFunc t a) = CT t
type instance Val t (a0, a1) = (Val t a0, Val t a1)
type instance Val t (a0, a1, a2) = (Val t a0, Val t a1, Val t a2)
type instance Val t (LongVec n a) = LongVec n (Val t a)
type instance Val t (Maybe a) = Maybe (Val t a)
type instance Val t (Stream a) = Stream (Val t a)
-- type instance Val t SimpleExpr = SimpleExpr 


--initBackProp :: forall a t. Multiplicative (CT a) => DFunc t a -> t -> CT t
----initBackProp lens = pullback lens (const one)
--initBackProp = flip pullback (const one)

initBackProp :: forall a t. Multiplicative (CT a) => Proxy a -> DFunc t a -> t -> CT t
--initBackProp lens = pullback lens (const one)
initBackProp _ = flip pullback (const one)


simpleDerivative :: forall a b. (Multiplicative (CT b)) =>
  (DFunc a a -> DFunc a b) -> 
  a -> 
  CT a
simpleDerivative f = initBackProp (Proxy @b) (f identity)

--simpleDerivative2 :: forall a b. (Multiplicative (CT b)) =>
--  (DFunc (DFunc a a) (DFunc a a) -> DFunc (DFunc a a) b) ->
--  DFunc a a ->
--  DFunc a (CT a)
--simpleDerivative2 = simpleDerivative
--
--simpleDerivative3 :: forall a b. (Multiplicative (CT b)) =>
--  (DFunc (DFunc (DFunc a a) (DFunc a a)) (DFunc (DFunc a a) (DFunc a a)) -> DFunc (DFunc (DFunc a a) (DFunc a a)) b) ->
--  DFunc (DFunc a a) (DFunc a a) ->
--  DFunc (DFunc a a) (DFunc a (CT a))
--simpleDerivative3 = simpleDerivative

type DifferentiableFunc a b = DFunc a a -> DFunc a b

--type family AddDiffOrder a :: Type
--type instance AddDiffOrder a = DFunc a a
--
--type TwiceDifferentiableFunc a b = DifferentiableFunc (AddDiffOrder a) b
--type family NDifferentiableFunc (n :: Peano) a b :: Type -- = DifferentiableFunc (AddDiffOrder a) b
--type instance NDifferentiableFunc Zero' a b = a -> b



secondSimpleDerivative :: (Multiplicative (CT b), Multiplicative (CT (CT a))) =>
  (DFunc (DFunc a a) (DFunc a a) -> DFunc (DFunc a a) b) ->
  a ->
  CT a
secondSimpleDerivative = simpleDerivative . simpleDerivative

--secondSimpleDerivative2 :: forall a b. (Multiplicative (CT b), Multiplicative (CT a), Multiplicative (CT (CT a)), Multiplicative (CT (CT (CT a))), Additive (CT a)) =>
--  (DFunc (DFunc (DFunc a a) (DFunc a a)) (DFunc (DFunc a a) (DFunc a a)) -> DFunc (DFunc (DFunc a a) (DFunc a a)) b) ->
--  DFunc a a ->
--  DFunc a (CT a)
--secondSimpleDerivative2 = simpleDerivative2 . simpleDerivative3


thirdSimpleDerivative :: (Multiplicative (CT b), Distributive a, a ~ CT a) =>
  (DFunc (DFunc (DFunc a a) (DFunc a a)) (DFunc (DFunc a a) (DFunc a a)) -> DFunc (DFunc (DFunc a a) (DFunc a a)) b) ->
  a ->
  CT a
thirdSimpleDerivative = simpleDerivative . simpleDerivative . simpleDerivative

fourthSimpleDerivative :: (Multiplicative (CT b), Distributive a, a ~ CT a) =>
  (DFunc (DFunc (DFunc (DFunc a a) (DFunc a a)) (DFunc (DFunc a a) (DFunc a a))) (DFunc (DFunc (DFunc a a) (DFunc a a)) (DFunc (DFunc a a) (DFunc a a))) -> DFunc (DFunc (DFunc (DFunc a a) (DFunc a a)) (DFunc (DFunc a a) (DFunc a a))) b) ->
  a ->
  CT a
fourthSimpleDerivative = simpleDerivative . simpleDerivative . simpleDerivative . simpleDerivative

-- VAL
type Differentiable t a = a -> t -> Val t a

--tupleVal :: Differentiable t a0 -> Differentiable t a1 -> Differentiable t (a0, a1)
--tupleVal f0 f1 (y0, y1) = fork (f0 y0) (f1 y1)

tupleVal :: (Differentiable t a0, Differentiable t a1) -> Differentiable t (a0, a1)
tupleVal (f0, f1) (y0, y1) = fork (f0 y0) (f1 y1)
-- tupleVal_ (f0, f1) = fork (cross f0 f1)

tripleVal :: (Differentiable t a0, Differentiable t a1, Differentiable t a2) -> Differentiable t (a0, a1, a2)
tripleVal (f0, f1, f2) (y0, y1, y2) = fork3 (f0 y0) (f1 y1) (f2 y2)

functorVal :: Functor f => Differentiable t a -> f a -> t -> f (Val t a)
functorVal = flip . (.) fmap . flip

longVecVal :: Differentiable t a -> Differentiable t (LongVec n a)
longVecVal = functorVal

maybeVal :: Differentiable t a -> Differentiable t (Maybe a)
maybeVal = functorVal

streamVal :: Differentiable t a -> Differentiable t (Stream a)
streamVal = functorVal

customValDerivative ::
  (b -> a -> Val a b) ->
  (DFunc a a -> b) -> -- (LongVec n0 (DFunc a a), LongVec n1 (DFunc a a))
  a ->
  Val a b
customValDerivative value f = value (f identity)

class HasDerivative a t | a -> t where
  diff :: Differentiable t a

derivative :: HasDerivative b t =>
  (DFunc a a -> b) ->
  t ->
  Val t b
derivative f = diff (f identity)

--derivative2 :: HasDerivative b (DFunc t t) =>
--  (DFunc a a -> b) ->
--  DFunc t t ->
--  Val (DFunc t t) b
--derivative2 = derivative
--
--secondDerivative :: forall a b t. (
--    HasDerivative b (DFunc t t), 
--    HasDerivative (Val (DFunc t t) b) t
--  ) =>
--  (DFunc a a -> b) ->
--  t ->
--  Val t b
--secondDerivative = derivative . derivative



--secondDerivative :: HasDerivative b t => 
--  (DFunc a a -> b) -> 
--  t -> 
--  Val t b
--secondDerivative f =      diff (f identity)


--derivatives :: HasDerivative b t => 
--  (DFunc a a -> b) -> 
--  t -> 
--  Val t b
--
--x = iterate

--derivativeN ::
--  (Isomorphism cat, CatBiFunctor (,) cat, StartBackprop cat x) =>
--  Natural ->
--  Backprop cat x x ->
--  Backprop cat x x
--derivativeN n f = iterate numba f !! fromIntegral n

instance Multiplicative (CT a) =>
  HasDerivative (DFunc t a) t where
    diff :: Differentiable t (DFunc t a)
    diff = initBackProp (Proxy @a)

instance (HasDerivative a0 t, HasDerivative a1 t) =>
  HasDerivative (a0, a1) t where
    diff :: Differentiable t (a0, a1)
    diff = tupleVal (diff, diff)

instance (HasDerivative a t) =>
  HasDerivative (LongVec n a) t where
    diff :: Differentiable t (LongVec n a)
    diff = longVecVal diff

instance (HasDerivative a t) =>
  HasDerivative (Maybe a) t where
    diff :: Differentiable t (Maybe a)
    diff = maybeVal diff

instance (HasDerivative a t) =>
  HasDerivative (Stream a) t where
    diff :: Differentiable t (Stream a)
    diff = streamVal diff



-- Examples:
longVecTupleVal :: Differentiable t a0 -> Differentiable t a1 -> Differentiable t (LongVec n0 a0, LongVec n1 a1)
longVecTupleVal d0 d1 = tupleVal (longVecVal d0, longVecVal d1)

--longVecTupleVal_ :: (Differentiable t a0, Differentiable t a1) -> Differentiable t (LongVec n0 a0, LongVec n1 a1)
--longVecTupleVal_ = uncurry tupleVal . cross longVecVal longVecVal

--initLongVecTupleVal :: (Multiplicative (CT a0), Multiplicative (CT a1)) => 
--  Differentiable t (LongVec n0 (DFunc t a0), LongVec n1 (DFunc t a1))
----initLongVecTupleVal = longVecTupleVal initBackProp initBackProp
--initLongVecTupleVal = tupleVal (longVecVal initBackProp, longVecVal initBackProp)

customDerivative ::
  (b -> a -> Val a b) ->
  (DFunc t t -> c) ->
  (c -> b) ->
  a ->
  Val a b
customDerivative value arg f = value ((f . arg) identity)


-- ARG

lensToTuple :: (Additive (CT a0), Additive (CT a1)) =>
  DFunc t (a0, a1) -> (DFunc t a0, DFunc t a1)
lensToTuple (MkDFunc d) = (MkDFunc d0, MkDFunc d1) where
  d0 = \t -> let
      ((x0, _), dxt) = d t
    in (x0, \dx0 -> dxt (dx0, zero))
  d1 = \t -> let
      ((_, x1), dxt) = d t
    in (x1, \dx1 -> dxt (zero, dx1))

lensToTriple :: (Additive (CT a0), Additive (CT a1), Additive (CT a2)) =>
  DFunc t (a0, a1, a2) -> (DFunc t a0, DFunc t a1, DFunc t a2)
lensToTriple (MkDFunc d) = (MkDFunc d0, MkDFunc d1, MkDFunc d2) where
  d0 = \t -> let
      ((x0, _, _), dxt) = d t
    in (x0, \dx0 -> dxt (dx0, zero, zero))
  d1 = \t -> let
      ((_, x1, _), dxt) = d t
    in (x1, \dx1 -> dxt (zero, dx1, zero))
  d2 = \t -> let
      ((_, _, x2), dxt) = d t
    in (x2, \dx2 -> dxt (zero, zero, dx2))

longVecBasis :: (DVG.Vector v a, KnownNat n) =>
  Finite n -> a -> a -> DVGS.Vector v n a
longVecBasis k zero' one' = DVGS.generate $ \l ->
  if k == l
    then one'
    else zero'

lensToLongVec :: forall t a n. (Additive (CT a), KnownNat n) =>
  DFunc t (LongVec n a) -> LongVec n (DFunc t a)
lensToLongVec (MkDFunc d) = DVGS.generate $ \k ->
  MkDFunc $ \t -> let
      (v, dvt) = d t :: (LongVec n a, LongVec n (CT a) -> CT t)
    in (DVGS.index v k, dvt . longVecBasis k zero) :: (a, CT a -> CT t)


--lensToLongVec :: forall t a n. (Additive (CT a), KnownNat n) =>
--  DFunc t (LongVec n a) -> LongVec n (DFunc t a)
lensToTupleLongVec :: forall n0 n1 a0 a1 t. (
    Additive (CT a0), 
    Additive (CT a1),
    Additive (LongVec n0 (CT a0)), 
    Additive (LongVec n1 (CT a1)), 
    KnownNat n0, 
    KnownNat n1
  ) =>
    DFunc t (LongVec n0 a0, LongVec n1 a1) -> 
    (LongVec n0 (DFunc t a0), LongVec n1 (DFunc t a1))
lensToTupleLongVec = cross lensToLongVec lensToLongVec . lensToTuple


--customArgDerivative :: (Multiplicative (CT c)) =>
--  (DFunc a a -> c) ->
--  (c -> DFunc a c)  -> 
--  a ->
--  CT a 
customArgDerivative :: forall a c d. Multiplicative (CT d) => 
  (DFunc a a -> c) -> 
  Proxy d ->
  (c -> DFunc a d) -> 
  a -> 
  CT a
customArgDerivative arg _ f = initBackProp (Proxy @d) ((f . arg) identity)

class HasGradient c t | c -> t where
  fromDFunc :: DFunc t t -> c

gradient :: forall a c d. (HasGradient c a, Multiplicative (CT d)) => 
--gradient :: (HasGradient c t, Multiplicative (CT d)) => 
  Proxy d ->
  (c -> DFunc a d) -> 
  a ->
  CT a
gradient _ = customArgDerivative fromDFunc (Proxy @d)

instance HasGradient (DFunc t t) t where
  fromDFunc = id

instance (Additive (CT a0), Additive (CT a1)) => 
  HasGradient (DFunc (a0, a1) a0, DFunc (a0, a1) a1) (a0, a1) where
    fromDFunc = lensToTuple


--derivativeXTuple :: forall a0 a1 b. (Additive (CT a0), Multiplicative (CT b)) =>
--  ((DFunc a0 a0, DFunc a0 a1) -> DFunc a0 b) ->
--  (a0, a1) ->
--  CT a0
--derivativeXTuple f (x0, x1) = initBackProp (Proxy @b) (f (identity, constDFunc x1)) x0
tupleXDerivative :: (Multiplicative (CT b), Additive (CT a0)) =>
  ((DFunc a0 a0, DFunc a0 a1) -> DFunc a0 b) ->
  (a0, a1) ->
  Val a0 (DFunc a0 b)
tupleXDerivative f (x0, x1) = diff (f (identity, constDFunc x1)) x0
tupleDerivativeOverX :: (HasDerivative b a0, Additive (CT a0)) =>
  ((DFunc a0 a0, DFunc a0 a1) -> b) ->
  (a0, a1) ->
  Val a0 b
tupleDerivativeOverX f (x0, x1) = diff (f (identity, constDFunc x1)) x0

tupleDerivativeOverY :: (HasDerivative b a1, Additive (CT a1)) =>
  ((DFunc a1 a0, DFunc a1 a1) -> b) ->
  (a0, a1) ->
  Val a1 b
tupleDerivativeOverY f (x0, x1) = diff (f (constDFunc x0, identity)) x1

derivative2ArgsOverX :: (HasDerivative b a0, Additive (CT a0)) =>
  (DFunc a0 a0 -> DFunc a0 a1 -> b) ->
  a0 ->
  a1 ->
  Val a0 b
derivative2ArgsOverX f x0 x1 = diff (f identity (constDFunc x1)) x0

derivative2ArgsOverY :: (HasDerivative b a1, Additive (CT a1)) =>
  (DFunc a1 a0 -> DFunc a1 a1 -> b) ->
  a0 ->
  a1 ->
  Val a1 b
derivative2ArgsOverY f x0 = diff (f (constDFunc x0) identity)

derivative3ArgsOverX :: (HasDerivative b a0, Additive (CT a0)) =>
  (DFunc a0 a0 -> DFunc a0 a1 -> DFunc a0 a2 -> b) ->
  a0 ->
  a1 ->
  a2 ->
  Val a0 b
derivative3ArgsOverX f x0 x1 x2 = diff (f identity (constDFunc x1) (constDFunc x2)) x0

derivative3ArgsOverY :: (HasDerivative b a1, Additive (CT a1)) =>
  (DFunc a1 a0 -> DFunc a1 a1 -> DFunc a1 a2 -> b) ->
  a0 ->
  a1 ->
  a2 ->
  Val a1 b
derivative3ArgsOverY f x0 x1 x2 = diff (f (constDFunc x0) identity (constDFunc x2)) x1

derivative3ArgsOverZ :: (HasDerivative b a2, Additive (CT a2)) =>
  (DFunc a2 a0 -> DFunc a2 a1 -> DFunc a2 a2 -> b) ->
  a0 ->
  a1 ->
  a2 ->
  Val a2 b
derivative3ArgsOverZ f x0 x1 = diff (f (constDFunc x0) (constDFunc x1) identity)




--derivativeTuple :: forall a. (Distributive (CT a)) =>
--  ((DFunc (a, a) a, DFunc (a, a) a) -> DFunc (a, a) a) ->
--  (a, a) ->
--  (CT a, CT a)
--derivativeTuple f =  initBackProp (Proxy @a) ((f . lensToTuple) identity)
--tupleDerivative :: (HasDerivative b a0, HasDerivative b a1) =>
--  ((DFunc (a0, a1) a0, DFunc (a0, a1) a1) -> b) ->
--  (a0, a1) ->
--  Val (a0, a1) b
tupleDerivative :: (HasDerivative b t, Additive (CT a0), Additive (CT a1)) =>
  ((DFunc (a0, a1) a0, DFunc (a0, a1) a1) -> b) ->
  t ->
  Val t b
tupleDerivative f = diff ((f . lensToTuple) identity)

longVecDerivative :: (Additive (CT a), KnownNat n, HasDerivative b t) =>
  (LongVec n (DFunc (LongVec n a) a) -> b) ->
  t ->
  Val t b
longVecDerivative f = diff ((f . lensToLongVec) identity)

-- _ = fmap constDFunc :: forall a n. Additive (CT a) => LongVec n a -> LongVec n (DFunc a a)
temp :: Additive (CT a) => Finite n -> LongVec n a -> LongVec n (DFunc a a)
temp k = DVGS.imap setVal where
  setVal l x = if k == l
    then identity
    else constDFunc x  

longVecDerivativeOverN :: forall n a b. (HasDerivative b a, Additive (CT a)) =>
  (LongVec n (DFunc a a) -> b) ->
  Finite n ->
  LongVec n a ->
  Val a b
longVecDerivativeOverN f k vec = diff (f basis) x where
  basis = DVGS.imap setVal vec :: LongVec n (DFunc a a) where
    setVal l x_ = if k == l
      then identity
      else constDFunc x_
  x = DVGS.index vec k :: a