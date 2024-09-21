{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-export-lists #-}


module Numeric.InfBackprop.Diff where


import Numeric.InfBackprop.Tangent (Tangent, Dual, CT, LongVec)
import GHC.Base (id, (.), ($), const, undefined, flip, Type, Maybe(Just, Nothing), fmap, Float)
import Data.Tuple (fst, uncurry, curry)
import NumHask ((*), (+), sin, cos, Additive, zero, Multiplicative, one, ExpField, exp, log, (**), sqrt, sin, cos, sinh,
    asin, acos, cosh, atan, atan2, asinh, acosh, atanh, pi,
    Subtractive, negate, (-),
    Divisive, recip, (/), Field, AdditiveAction, (^), (^^), TrigField, two, Distributive, Integral, fromIntegral,
    FromIntegral
  )
import Data.Type.Equality (type (~))
import NumHask.Extra (IntegralPower, integralPow)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Stream as DS
import Debug.SimpleExpr (SimpleExpr)
import Data.Finite (Finite)
import GHC.TypeNats (KnownNat)
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic as DVG
import NumHask.Extra (longVecBasis)
import Prelude.Tools (curry3, uncurry3)
import qualified Data.FiniteList as DF


data Diff t a = forall h. MkDiff {value :: a, backpropCache :: h -> CT a -> CT t, cache :: h}

backprop :: Diff t a -> CT a -> CT t
backprop (MkDiff _ bpc h) = bpc h

initDiff :: a -> Diff a a
initDiff x = MkDiff x (const id) ()

call :: (Diff a a -> Diff a b) -> a -> b
call f = value . f . initDiff

type instance Tangent (Diff a b) = Diff a (Tangent b)
type instance Dual (Diff a b) = Diff a (Dual b)

derivativeOp :: (Diff a a -> Diff a b) ->
  a ->
  CT b ->
  CT a
derivativeOp f = backprop . f . initDiff

simpleDerivative :: forall a b. (Multiplicative (CT b)) =>
  (Diff a a -> Diff a b) ->
  a ->
  CT a
simpleDerivative f x = derivativeOp f x one




-- Common
class DerivativeWith a where
  initBackProp :: a -> CT a

instance (DerivativeWith a, Additive (CT t)) =>
  DerivativeWith (Diff t a) where
    initBackProp (MkDiff x _ _) = constDiff (initBackProp x)

instance DerivativeWith Float where
  initBackProp = const 1

instance DerivativeWith SimpleExpr where
  initBackProp = const one


--type instance Val a (b0, b1, b2) = (Val a b0, Val a b1, Val a b2)
--type instance Val a (Maybe b) = Maybe (Val a b)
--type instance Val a (Stream b) = Stream (Val a b)
-- type instance Val t SimpleExpr = SimpleExpr


type Differentiable a b = a -> b -> DiffOutput a b

class HasDerivative a b | b -> a where
  type DiffOutput a b :: Type
  diff :: Differentiable a b

instance DerivativeWith b =>
  HasDerivative a (Diff a b) where
    type DiffOutput a (Diff a b) = CT a
    diff :: Differentiable a (Diff a b)
    diff _ (MkDiff y bp h) = bp h $ initBackProp y

derivative :: HasDerivative a b =>
  (Diff a a -> b) ->
  a ->
  DiffOutput a b
derivative f x = diff x (f (initDiff x))
--derivative f x = simpleDerivative ((diff x) f)


customArgDerivative :: HasDerivative a b =>
  (Diff a a -> c) ->
  (c -> b) ->
  a ->
  DiffOutput a b
customArgDerivative arg f = derivative (f . arg)


class HasGradient a c | c -> a where
  anyArg :: Diff a a -> c

instance HasGradient a (Diff a a) where
  anyArg = id

gradient :: forall b c a. (HasGradient a c, Multiplicative (CT b)) =>
  Proxy b ->
  (c -> Diff a b) ->
  a ->
  CT a
gradient _ f = simpleDerivative (f . anyArg)

customValueGradient :: (HasGradient a c, Multiplicative (CT b)) =>
  (b -> Diff a b) ->
  (c -> b) ->
  a ->
  CT a
customValueGradient value_ f = simpleDerivative (value_ . f . anyArg)

autoDiff :: (HasDerivative a b, HasGradient a c) =>
  (c -> b) ->
  a ->
  DiffOutput a b
autoDiff f = derivative (f . anyArg)

--Tuples
instance (HasDerivative a b0, HasDerivative a b1) =>
  HasDerivative a (b0, b1) where
    type DiffOutput a (b0, b1) = (DiffOutput a b0, DiffOutput a b1)
    diff :: Differentiable a (b0, b1)
    diff x (y0, y1) = (diff x y0, diff x y1)

tupleArg :: (Additive (CT a0), Additive (CT a1)) =>
  Diff t (a0, a1) -> (Diff t a0, Diff t a1)
tupleArg (MkDiff (x0, x1) bpc h) = (
    MkDiff x0 (\h_ cy -> bpc h_ (cy, zero)) h,
    MkDiff x1 (\h_ cy -> bpc h_ (zero, cy)) h
  )

instance (Additive (CT a0), Additive (CT a1)) =>
  HasGradient (a0, a1) (Diff (a0, a1) a0, Diff (a0, a1) a1)  where
    anyArg = tupleArg

tupleDerivative :: (HasDerivative (a0, a1) b, Additive (CT a0), Additive (CT a1)) =>
  ((Diff (a0, a1) a0, Diff (a0, a1) a1) -> b) ->
  (a0, a1) ->
  DiffOutput (a0, a1) b
tupleDerivative = customArgDerivative tupleArg

tupleDerivativeOverX :: (HasDerivative a0 b, Additive (CT a0)) =>
  ((Diff a0 a0, Diff a0 a1) -> b) ->
  (a0, a1) ->
  DiffOutput a0 b
tupleDerivativeOverX f (x0, x1) = derivative (\x -> f (x, constDiff x1)) x0

tupleDerivativeOverY :: (HasDerivative a1 b, Additive (CT a1)) =>
  ((Diff a1 a0, Diff a1 a1) -> b) ->
  (a0, a1) ->
  DiffOutput a1 b
tupleDerivativeOverY f (x0, x1) = derivative (\x -> f (constDiff x0, x)) x1

twoArgsDerivative :: (HasDerivative (a0 ,a1) b, Additive (CT a0), Additive (CT a1)) =>
  (Diff (a0, a1) a0 -> Diff (a0, a1) a1 -> b) ->
  a0 ->
  a1 ->
  DiffOutput (a0, a1) b
twoArgsDerivative f = curry (derivative $ uncurry f . tupleArg)

derivative2ArgsOverX :: (HasDerivative a0 b, Additive (CT a0)) =>
  (Diff a0 a0 -> Diff a0 a1 -> b) ->
  a0 ->
  a1 ->
  DiffOutput a0 b
derivative2ArgsOverX f x0 x1 = derivative (\x -> f x (constDiff x1)) x0     -- diff (f identity (constDFunc x1)) x0

derivative2ArgsOverY :: (HasDerivative a1 b, Additive (CT a1)) =>
  (Diff a1 a0 -> Diff a1 a1 -> b) ->
  a0 ->
  a1 ->
  DiffOutput a1 b
derivative2ArgsOverY f x0 = derivative (f (constDiff x0))

-- Triples
tripleArg :: (Additive (CT a0), Additive (CT a1), Additive (CT a2)) =>
  Diff t (a0, a1, a2) -> (Diff t a0, Diff t a1, Diff t a2)
tripleArg (MkDiff (x0, x1, x2) bpc h) = (
    MkDiff x0 (\h_ cy -> bpc h_ (cy, zero, zero)) h,
    MkDiff x1 (\h_ cy -> bpc h_ (zero, cy, zero)) h,
    MkDiff x2 (\h_ cy -> bpc h_ (zero, zero, cy)) h
  )

tripleDerivative :: (HasDerivative (a0, a1, a2) b, Additive (CT a0), Additive (CT a1), Additive (CT a2)) =>
  ((Diff (a0, a1, a2) a0, Diff (a0, a1, a2) a1, Diff (a0, a1, a2) a2) -> b) ->
  (a0, a1, a2) ->
  DiffOutput (a0, a1, a2) b
tripleDerivative = customArgDerivative tripleArg

instance (Additive (CT a0), Additive (CT a1), Additive (CT a2)) =>
  HasGradient (a0, a1, a2) (Diff (a0, a1, a2) a0, Diff (a0, a1, a2) a1, Diff (a0, a1, a2) a2) where
    anyArg = tripleArg

threeArgsDerivative :: (HasDerivative (a0, a1, a2) b, Additive (CT a0), Additive (CT a1), Additive (CT a2)) =>
  (Diff (a0, a1, a2) a0 -> Diff (a0, a1, a2) a1 -> Diff (a0, a1, a2) a2 -> b) ->
  a0 ->
  a1 ->
  a2 ->
  DiffOutput (a0, a1, a2) b
threeArgsDerivative f = curry3 (derivative $ uncurry3 f . tripleArg)

derivative3ArgsOverX :: (HasDerivative a0 b, Additive (CT a0)) =>
  (Diff a0 a0 -> Diff a0 a1 -> Diff a0 a2 -> b) ->
  a0 ->
  a1 ->
  a2 ->
  DiffOutput a0 b
derivative3ArgsOverX f x0 x1 x2 = derivative (\x -> f x (constDiff x1) (constDiff x2)) x0 -- diff (f identity (constDFunc x1) (constDFunc x2)) x0

derivative3ArgsOverY :: (HasDerivative a1 b, Additive (CT a1)) =>
  (Diff a1 a0 -> Diff a1 a1 -> Diff a1 a2 -> b) ->
  a0 ->
  a1 ->
  a2 ->
  DiffOutput a1 b
derivative3ArgsOverY f x0 x1 x2 = derivative (\x -> f (constDiff x0) x (constDiff x2)) x1 -- diff (f (constDFunc x0) identity (constDFunc x2)) x1

derivative3ArgsOverZ :: (HasDerivative a2 b, Additive (CT a2)) =>
  (Diff a2 a0 -> Diff a2 a1 -> Diff a2 a2 -> b) ->
  a0 ->
  a1 ->
  a2 ->
  DiffOutput a2 b
derivative3ArgsOverZ f x0 x1 = derivative (f (constDiff x0) (constDiff x1)) -- diff (f (constDFunc x0) (constDFunc x1) identity)


-- LongVec
instance (HasDerivative a b) =>
  HasDerivative a (LongVec n b) where
    type DiffOutput a (LongVec n b) = LongVec n (DiffOutput a b)
    diff :: Differentiable a (LongVec n b)
    diff = fmap . diff

longVecArg :: forall t a n. (Additive (CT a), KnownNat n) =>
  Diff t (LongVec n a) -> LongVec n (Diff t a)
longVecArg (MkDiff vec bpc h) = DVGS.generate $ \k ->
  MkDiff (DVGS.index vec k) (\h_ cy -> bpc h_ (longVecBasis k zero cy)) h

instance (KnownNat n, Additive (CT a)) =>
  HasGradient (LongVec n a) (LongVec n (Diff (LongVec n a) a)) where
    anyArg = longVecArg

vecDerivative :: (KnownNat n, HasDerivative (LongVec n a) b, Additive (CT a)) =>
  (LongVec n (Diff (LongVec n a) a) -> b) ->
  LongVec n a ->
  DiffOutput (LongVec n a) b
vecDerivative = customArgDerivative longVecArg

-- Stream
instance (HasDerivative a b) =>
  HasDerivative a (DS.Stream b) where
    type DiffOutput a (DS.Stream b) = DS.Stream (DiffOutput a b)
    diff :: Differentiable a (DS.Stream b)
    diff = fmap . diff

streamArg :: (Additive (CT a)) =>
  Diff t (DS.Stream a) -> DS.Stream (Diff t a)
streamArg (MkDiff x bpc h) = DS.Cons
    (MkDiff x_head bpc_head h)
    (streamArg (MkDiff x_tail bpc_tail h))
  where
  x_head = DS.head x
  x_tail = DS.tail x
  bpc_head h_ = bpc h_ . DF.unit
  bpc_tail h_ = bpc h_ . DF.bTail

instance (Additive (CT a)) =>
  HasGradient (DS.Stream a) (DS.Stream (Diff (DS.Stream a) a)) where
    anyArg = streamArg

streamDerivative :: (HasDerivative (DS.Stream a) b, Additive (CT a)) =>
  (DS.Stream (Diff (DS.Stream a) a) -> b) ->
  DS.Stream a ->
  DiffOutput (DS.Stream a) b
streamDerivative = customArgDerivative streamArg

-- Maybe
instance (HasDerivative a b) =>
  HasDerivative a (Maybe b) where
    type DiffOutput a (Maybe b) = Maybe (DiffOutput a b)
    diff :: Differentiable a (Maybe b)
    diff = fmap . diff

maybeArg :: Diff t (Maybe a) -> Maybe (Diff t a)
maybeArg (MkDiff maybeX bpc h) = case maybeX of
  Just x -> Just (MkDiff x (\h_ -> bpc h_ . Just) h)
  Nothing -> Nothing

instance HasGradient (Maybe a) (Maybe (Diff (Maybe a) a)) where
  anyArg = maybeArg

maybeDerivative :: (HasDerivative (Maybe a) b) =>
  (Maybe (Diff (Maybe a) a) -> b) ->
  Maybe a ->
  DiffOutput (Maybe a) b
maybeDerivative = customArgDerivative maybeArg




-- Funcs
simpledFunc :: Multiplicative (CT a) =>
  (a -> a) -> (a -> CT a) -> Diff t a -> Diff t a
simpledFunc f f' (MkDiff x bpc h) = MkDiff (f x) (\h_ cy -> bpc h_ $ f' x * cy) h

constDiff :: Additive (CT t) => 
  a -> Diff t a
constDiff x = MkDiff x (const zero) ()

constDFunc :: (Additive (CT a)) =>
  b -> Diff t a -> Diff t b
constDFunc x (MkDiff _ bpc h) = MkDiff x (\h_ _ -> bpc h_ zero) h


-- Distributive
instance (Additive (CT t), Additive a) =>
  Additive (Diff t a) where
    zero = constDiff zero
    (MkDiff x bpx hx) + (MkDiff y bpy hy) = MkDiff (x + y) (\(hx_, hy_) cz -> bpx hx_ cz + bpy hy_ cz) (hx, hy)

instance (Additive (CT t), Subtractive a, Distributive (CT a), Subtractive (CT a)) =>
  Subtractive (Diff t a) where
    negate = simpledFunc negate (const $ negate one)
    (MkDiff x bpx hx) - (MkDiff y bpy hy) = MkDiff (x - y) (\(hx_, hy_) cz -> bpx hx_ cz + bpy hy_ (negate cz)) (hx, hy)

instance (Additive (CT t), Multiplicative a, a ~ CT a) =>
  Multiplicative (Diff t a) where
    one = constDiff one
    (MkDiff x bpx hx) * (MkDiff y bpy hy) = MkDiff (x * y) (\(hx_, hy_) cz -> bpx hx_ (cz * y) + bpy hy_ (x * cz)) (hx, hy)

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
  Divisive (Diff t a) where
    recip (MkDiff x bpc h) = MkDiff
      r
      (\(r_, h_) cy -> bpc h_ $ negate $ r_^2 * cy )
      (r, h)
      where r = recip x

instance (FromIntegral a b, Additive (CT t)) =>
  FromIntegral (Diff t a) b where
    fromIntegral :: b -> Diff t a
    fromIntegral x = constDiff (fromIntegral x)


-- Pow
instance (Additive (CT t), Subtractive a, IntegralPower p a, FromIntegral a p, CT a ~ a) =>
  IntegralPower p (Diff t a) where
    integralPow n = simpledFunc (integralPow n) (\x -> fromIntegral n * integralPow (n - one) x)


-- Field
#if MIN_VERSION_numhask(0,11,0)
#else
instance (Additive ct, Field a, a ~ CT a) =>
  Field (Diff t a)
#endif

--sqrtC :: (ExpField a, a ~ CT a) => Diff t a -> Diff t a
--sqrtC (MkDiff x bp) = MkDiff y (bp . recip . (two *) . (y *)) where
--  y = sqrt x

instance (ExpField a, Additive (CT t), CT a ~ a) =>
  ExpField (Diff t a) where
    exp = simpledFunc exp exp
    log = simpledFunc log recip
    (MkDiff x bpx hx) ** (MkDiff n bpn hn) =
      MkDiff y (\(y_, hx_, hn_) cy -> bpx hx_ (bp1 * cy) + bpn hn_ (bp2 y_ * cy)) (y, hx, hn) where
        bp1 = n * (x ** (n - one))
        bp2 y_= log x * y_
        y = x ** n
    sqrt (MkDiff x bp h) = MkDiff y (\(y_, h_) cy -> bp h_ $ recip $ two * y_ * cy) (y, h) where
      y = sqrt x

instance (Additive (CT t), ExpField a, TrigField a, a ~ CT a) =>
  TrigField (Diff t a) where
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
  ToConst a (Diff t b) where
    stopDiff = constDiff . stopDiff

_ = stopDiff (42 :: Float) :: Float

temp1 :: forall t. Additive (CT t) =>
  Diff t Float
temp1 = stopDiff (42 :: Float)

temp2 :: forall t1 t2. (Additive (CT t1), Additive (CT t2)) =>
  Diff t2 (Diff t1 Float)
temp2 = stopDiff (42 :: Float)