{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-export-lists #-}


module Numeric.InfBackprop.DFunc2 where

import Data.Profunctor (Profunctor, dimap, Strong, first', second', Costrong, unfirst, unsecond)
import NumHask ((*), (+), sin, cos, Additive, zero, Multiplicative, one, ExpField, exp, log, (**), sqrt, sin, cos, sinh,
    asin, acos, cosh, atan, atan2, asinh, acosh, atanh, pi,
    Subtractive, negate, (-),
    Divisive, recip, (/), Field, AdditiveAction, (^), (^^), TrigField, two, Distributive, Integral, fromIntegral, FromIntegral
  )
import Prelude (fst, ($), snd, const, curry, uncurry, Int, Ord, Float, Double, Bool, Integer, Word, undefined, Functor, fmap, flip)
import qualified Prelude as P
import Data.Bifunctor (bimap)
import Prelude.Tools (fork, cross, assoc, disassoc)
import Optics (Iso', iso)
import Data.Functor.Classes (Show1)
import Data.Kind (Type)
import Data.Tuple.Extra (curry3)
import GHC.Natural (Natural)
import GHC.Int (Int8, Int16, Int32, Int64)
import GHC.Word (Word8, Word16, Word32, Word64)
import Debug.SimpleExpr (SimpleExpr, number)
import Data.Data (Proxy(Proxy))
import GHC.TypeNats (Nat)
import Control.PolyFunctor (binarryFfmap1, BinnaryVectorizable1)
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector as DV
import qualified Data.Vector.Fixed.Boxed as DVFB
import qualified Data.Vector.Fixed as DVF
import Numeric.InfBackprop.Tangent (Tangent, Dual, CT)
import Control.Category (Category, id, (.))
import Control.Composition ((&))
import Data.Foldable (foldl')
import Data.Type.Equality (type (~))
import NumHask.Extra (IntegralPower, integralPow)


type LongVec (n :: Nat) a = DVGS.Vector DV.Vector n a


newtype DFunc a b = MkDFunc {unpackLensD :: a -> (b, CT b -> CT a)}

view :: DFunc a b -> a -> b
view (MkDFunc x) = fst . x

update :: DFunc a b -> a -> CT b -> CT a
update (MkDFunc x) = snd . x

--data DFunc x y = forall cache. MkDFunc {
--    forward :: x -> (y, cache),
--    backward :: (CT y, cache) -> CT x
--  }
--type DFunc x y = DFunc (CT x) x (CT y) y

--type instance Tangent1 (DFunc x y) b = DFunc x (Tangent1 y b)
--type instance Dual (DFunc x y) = DFunc x (Dual y)
--type instance Tangent1 (DFunc a b) c = DFunc a (Tangent1 b c)
type instance Tangent (DFunc a b) = DFunc a (Tangent b)
type instance Dual (DFunc a b) = DFunc a (Dual b)


--call :: DFunc x y -> x -> y
--call (MkDFunc f _) = fst . f

--dFuncToDerivative :: DFunc a b -> (b -> CT b) -> a -> CT a
--dFuncToDerivative (MkDFunc f b) cvf = b . cross cvf id . f
pullback :: DFunc a b -> (b -> CT b) -> a -> CT a
pullback (MkDFunc l) my x = bp (my y) where
  (y, bp) = l x

identity :: DFunc a a
identity = MkDFunc (, id)

(%) :: DFunc b c -> DFunc a b -> DFunc a c
(MkDFunc d2) % (MkDFunc d1) = MkDFunc $ \x -> let
    (y, dyx) = d1 x
    (z, dzy) = d2 y
    dzx = dyx . dzy
  in (z, dzx)

--lensToDerivativePair :: DFunc a b -> a -> (b, CT b -> CT a)
--lensToDerivativePair (MkDFunc f b) x = (y, bp) where
--  bp cy = b (cy, h)
--  (y, h) = f x

instance Category DFunc where
  id = identity
  (.) = (%)
--  (MkDLens (f1 :: y -> (z, h1)) (b1 :: (cz, h1) -> cy)) . (MkDLens (f2 :: x -> (y, h2)) (b2 :: (cy, h2) -> cx)) = MkDFunc f3 b3 where
--    f3 :: x -> (z, (h1, h2))
--    f3 = assoc . cross f1 id . f2
--    b3 :: (cz, (h1, h2)) -> cx
--    b3 = b2 . cross b1 id . disassoc
    
--derivativePair :: forall a b.
--  (DFunc a a -> DFunc a b) ->
--  a ->
--  (b, CT b -> CT a)
--derivativePair func = lensToDerivativePair (func id)

derivativeOp ::
  (DFunc a a -> DFunc a b) ->
  (b -> CT b) ->
  a ->
  CT a
derivativeOp f sb x = dyx (sb y) where
  (y, dyx) = unpackLensD (f identity) x

--lensToUnnaryFunc :: DFunc x y -> DFunc t x -> DFunc t y
--lensToUnnaryFunc = (.)

scalarDFunc :: (Multiplicative a, a ~ CT a) =>
  (a -> a) -> (a -> a) -> DFunc a a
scalarDFunc f f' = MkDFunc $ \x -> (f x, \dy -> f' x * dy)
--scalarDFunc f f' = MkDFunc (fork f f') (uncurry (*))

constDLens :: (Additive (CT t)) =>
  a -> DFunc t a
constDLens c = MkDFunc $ const (c, const zero)

constDFunc :: (Additive (CT t)) => 
  a -> DFunc t a
constDFunc = constDLens

-- | Optimized wrt lensCtoP
lensToUnnaryFunc :: DFunc a b -> DFunc t a -> DFunc t b
lensToUnnaryFunc = (%)

lensToBinaryFunc :: Additive (CT t) =>
  DFunc (x0, x1) y -> DFunc t x0 -> DFunc t x1 -> DFunc t y
lensToBinaryFunc lens = curry $ lensToUnnaryFunc lens . tupleToLens


-- tuple
tupleToLens :: Additive (CT t) =>
  (DFunc t x0, DFunc t x1) -> DFunc t (x0, x1)
tupleToLens (MkDFunc d0, MkDFunc d1) = MkDFunc $ \t -> let
    (x0, dxt0) = d0 t
    (x1, dxt1) = d1 t
  in ((x0, x1), \(dx0, dx1) -> dxt0 dx0 + dxt1 dx1)

tripleToLens :: Additive (CT t) =>
  (DFunc t a0, DFunc t a1, DFunc t a2) -> DFunc t (a0, a1, a2)
tripleToLens (MkDFunc a0, MkDFunc a1, MkDFunc a2) = MkDFunc $ \t -> let
    (x0, dxt0) = a0 t
    (x1, dxt1) = a1 t
    (x2, dxt2) = a2 t
  in ((x0, x1, x2), \(dx0, dx1, dx2) -> dxt0 dx0 + dxt1 dx1 + dxt2 dx2)



--instance (
--    Additive x,
--    Additive (CT t)
--  ) =>
--    Additive (DFunc t x) where
--      (MkDFunc (f0 :: t -> (x, h1)) (b0 :: (cx, h1) -> ct)) + (MkDFunc (f1 :: t -> (x, h2)) (b1 :: (cx, h2) -> ct)) = MkDFunc f b where
--        f t = (y0 + y1, (h0, h1)) where
--          (y0, h0) = f0 t
--          (y1, h1) = f1 t
--        b (cy, (h0, h1)) = ct0 + ct1 where
--          ct0 = b0 (cy, h0)
--          ct1 = b1 (cy, h1)
--      zero = undefined -- constDFunc zero

--instance (Subtractive x, Additive (CT t), Subtractive (CT x)) =>
--  Subtractive (DFunc t x) where
--    (MkDFunc (f1 :: t -> (x, h1)) (b1 :: (cx, h1) -> ct)) - (MkDFunc (f2 :: t -> (x, h2)) (b2 :: (cx, h2) -> ct)) = MkDFunc f b where
--      f t = (y1 - y2, (h1, h2)) where
--        (y1, h1) = f1 t
--        (y2, h2) = f2 t
--      b (cy, (h1, h2)) = b1 (cy, h1) + b2 (negate cy, h2)
--    negate (MkDFunc f b) = MkDFunc (cross negate id . f) (b . cross negate id)
--
--instance (
--    Multiplicative a,
--    Additive (CT t),
--    a ~ CT a
--  ) =>
--    Multiplicative (DFunc t a) where
--      (MkDFunc (f1 :: t -> (a, h1)) (b1 :: (a, h1) -> ct)) * (MkDFunc (f2 :: t -> (a, h2)) (b2 :: (a, h2) -> ct)) = MkDFunc f b where
--        f t = (y1 * y2, (h1, y1, h2, y2)) where
--          (y1, h1) = f1 t
--          (y2, h2) = f2 t
--        b (cy, (h1, y1, h2, y2)) = b1 (cy * y2, h1) + b2 (y1 * cy, h2)
--      one = constDFunc one
--
--instance (
--    Divisive a,
--    Subtractive a,
--    Additive (CT t),
--    a ~ CT a
--  ) =>
--    Divisive (DFunc t a) where
--      (MkDFunc (f1 :: t -> (x, h1)) (b1 :: (cx, h1) -> ct)) / (MkDFunc (f2 :: t -> (x, h2)) (b2 :: (cx, h2) -> ct)) = MkDFunc f b where
--        f t = (y1 / y2, (h1, y1, h2, y2)) where
--          (y1, h1) = f1 t
--          (y2, h2) = f2 t
--        b (cy, (h1, y1, h2, y2)) = b1 (cy / y2, h1) + b2 (negate y1 * cy / (y2 * y2), h2)
--
--instance (
--    Additive (CT t),
--    Subtractive a,
--    TrigField a,
--    a ~ CT a
--  ) =>
--    TrigField (DFunc t a) where
--      sin = lensToUnnaryFunc $ scalarDFunc sin cos
--      cos = lensToUnnaryFunc $ scalarDFunc cos (negate . sin)
--      asin = undefined
--      acos = undefined
--      atan = undefined
--      atan2 = undefined
--      sinh = lensToUnnaryFunc $ scalarDFunc cosh sinh
--      cosh = lensToUnnaryFunc $ scalarDFunc sinh cosh
--      asinh = undefined
--      acosh = undefined
--      atanh = undefined
--      pi = constDFunc pi

longVecSum :: Additive a => LongVec n a -> a
longVecSum = foldl' (+) zero

-- Sum
sumLens :: (Additive a) =>
  DFunc (a, a) a
sumLens = MkDFunc $ \(x0, x1) -> (x0 + x1, \cy -> (cy, cy))

instance (Additive x, Additive (CT t)) =>
  Additive (DFunc t x) where
    zero = MkDFunc $ const (zero, zero)
    (+) = curry $ lensToUnnaryFunc sumLens . tupleToLens

subLens :: (Subtractive a, Subtractive (CT a)) =>
  DFunc (a, a) a
subLens = MkDFunc $ \(x0, x1) -> (x0 - x1, \dy -> (dy, negate dy))

instance (Subtractive b, Subtractive (CT b), Additive (CT t)) => -- , Subtractive dx) =>
  Subtractive (DFunc t b) where
    negate (MkDFunc a) = MkDFunc $ \t -> let (y, dyt) = a t in (negate y, dyt . negate)
    (-) = curry $ lensToUnnaryFunc subLens . tupleToLens


-- Mult

-- | only simple mutiplication in the same space
productLens :: (Multiplicative a, a ~ CT a) =>
  DFunc t (a, a) -> DFunc t a
productLens (MkDFunc a) = MkDFunc $ \t -> let
    ((x1, x2), dyx) = a t
  in (x1 * x2, \dy -> dyx (dy * x2, x1 * dy))

instance (Additive (CT t), Multiplicative a, a ~ CT a) =>
  Multiplicative (DFunc t a) where
    one = constDLens one
    (*) = curry $ productLens . tupleToLens


#if MIN_VERSION_numhask(0,11,0)
#else
instance (Additive ca, Distributive cb, b ~ cb) =>
  Distributive (LensD ca a cb b)
#endif


-- Divisible
square :: Multiplicative a => a -> a
square a = a * a

squareDLens :: (Distributive a, CT a ~ a) =>
  DFunc a a
squareDLens = scalarDFunc square (two *)

recipDFunc :: (Divisive a, Subtractive a, CT a ~ a) =>
  DFunc a a
recipDFunc = scalarDFunc recip (\x -> negate $ recip x^2)

instance (Additive (CT t), Divisive a, Subtractive a, CT a ~ a) =>
  Divisive (DFunc t a) where
    recip = lensToUnnaryFunc recipDFunc

instance (FromIntegral a b, Additive (CT t)) =>
  FromIntegral (DFunc t a) b where
    fromIntegral :: b -> DFunc t a
    fromIntegral = constDLens . fromIntegral


-- Pow


integralPowC :: (Divisive a, Ord p, Subtractive p, Integral p, FromIntegral a p, CT a ~ a) =>
  p -> DFunc a a
integralPowC n = scalarDFunc (^^ n) (\x -> fromIntegral n * (x ^^ (n - one)))

instance (Additive (CT t), Subtractive a, IntegralPower p a, FromIntegral a p, CT a ~ a) =>
  IntegralPower p (DFunc t a) where
    integralPow n = lensToUnnaryFunc (integralPowC n)




-- Field
#if MIN_VERSION_numhask(0,11,0)
#else
instance (Additive ct, Field a, a ~ CT a) =>
  Field (LensD dt t x x)
#endif

sqrtC :: (ExpField a, a ~ CT a) => DFunc a a
sqrtC = scalarDFunc sqrt (recip . (two *) . sqrt)

expC :: (ExpField a, a ~ CT a) => DFunc a a
expC = scalarDFunc exp exp

-- | wrong for negative x
logC :: (ExpField a, a ~ CT a) =>
  DFunc a a
logC = scalarDFunc log recip

powC :: (ExpField a, a ~ CT a) =>
  DFunc (a, a) a
powC = MkDFunc $ \(x, n) -> let xn = x ** n in (xn, \dy -> (n * (x ** (n - one)) * dy, log x * xn * dy))


instance (ExpField a, Additive (CT t), CT a ~ a) =>
  ExpField (DFunc t a) where
    exp = lensToUnnaryFunc expC
    log = lensToUnnaryFunc logC
    (**) = lensToBinaryFunc powC
    sqrt = lensToUnnaryFunc sqrtC

sinC :: (TrigField a, a ~ CT a) =>
  DFunc a a
sinC = scalarDFunc sin cos

cosC :: (TrigField a, a ~ CT a) =>
  DFunc a a
cosC = scalarDFunc cos (negate . sin)

asinC :: (ExpField a, TrigField a, a ~ CT a) =>
  DFunc a a
asinC = scalarDFunc asin (recip . sqrt . (one -) . square)

-- | Backprop lens version of arccos.
acosC :: (ExpField a, TrigField a, a ~ CT a) =>
  DFunc a a
acosC = scalarDFunc acos (negate . recip . sqrt . (one -) . square)

atanC :: (TrigField a, a ~ CT a) =>
  DFunc a a
atanC = scalarDFunc atan (recip . (one +) . square)

atan2C :: (TrigField a, a ~ CT a) =>
  DFunc (a, a) a
atan2C = MkDFunc $ \(x, y) -> (atan2 x y, \dy -> let t = dy / (square x + square y) in (y * t, negate $ x * t))

sinhC :: (TrigField a, a ~ CT a) =>
  DFunc a a
sinhC = MkDFunc $ \x -> (sinh x, \dy -> cosh x * dy)

coshC :: (TrigField a, a ~ CT a) =>
  DFunc a a
coshC = MkDFunc $ \x -> (cosh x, \dy -> sinh x * dy)

asinhC :: (ExpField a, TrigField a, a ~ CT a) =>
  DFunc a a
asinhC = scalarDFunc asinh (recip . sqrt . (one +) . square)

acoshC :: (ExpField a, TrigField a, a ~ CT a) =>
  DFunc a a
acoshC = scalarDFunc asinh (recip . sqrt . (\x -> x - one) . square)

atanhC :: (TrigField a, a ~ CT a) =>
  DFunc a a
atanhC = scalarDFunc atanh (recip . (one -) . square)

instance (Additive (CT t), ExpField a, TrigField a, a ~ CT a) =>
  TrigField (DFunc t a) where
    sin = lensToUnnaryFunc sinC
    cos = lensToUnnaryFunc cosC
    asin = lensToUnnaryFunc asinC
    acos = lensToUnnaryFunc acosC
    atan = lensToUnnaryFunc atanC
    atan2 = lensToBinaryFunc atan2C
    sinh = lensToUnnaryFunc sinhC
    cosh = lensToUnnaryFunc coshC
    asinh = lensToUnnaryFunc asinhC
    acosh = lensToUnnaryFunc acoshC
    atanh = lensToUnnaryFunc atanhC
    pi = constDLens pi


class Constantable a b c d | a b c -> d, a b d -> c where 
  constant :: Proxy a -> b -> c -> d

instance Constantable a b a b where
  constant _ b = const b 

instance (Additive (CT t), Constantable a b c d) => 
  Constantable a b (DFunc t c) (DFunc t d) where
    constant _ b f = MkDFunc $ \t -> (constant (Proxy @a) b (view f t), const zero)
  
_ = constant (Proxy @Float) (42 :: Float) :: Float -> Float
temp10 :: forall t. Additive (CT t) => DFunc t Float -> DFunc t Float
temp10 = constant (Proxy @Float) (42 :: Float) 
-- temp10 = constant @Float (42 :: Float) 


class ToConst a b where
  stopDiff :: a -> b

instance ToConst a a where
  stopDiff = id

instance (Additive (CT t), ToConst a b) =>
  ToConst a (DFunc t b) where
    stopDiff = constDFunc . stopDiff

_ = stopDiff (42 :: Float) :: Float 

temp1 :: forall t. Additive (CT t) => 
  DFunc t Float 
temp1 = stopDiff (42 :: Float)

temp2 :: forall t1 t2. (Additive (CT t1), Additive (CT t2)) => 
  DFunc t2 (DFunc t1 Float) 
temp2 = stopDiff (42 :: Float)

--instance (Basis a, Additive dt) =>
--  Basis (DFunc dt t da a) where
--    type End (DFunc dt t da a) b = End a b
--    initBackProp :: forall b. (DFunc dt t da a -> b) -> End a b
--    initBackProp bp = initBackProp (bp . constC_)
--    zeroBackProp :: DFunc dt t da a
--    zeroBackProp = constC_ zeroBackProp