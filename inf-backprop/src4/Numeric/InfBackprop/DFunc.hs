{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-export-lists #-}

module Numeric.InfBackprop.DFunc where

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


type LongVec (n :: Nat) a = DVGS.Vector DV.Vector n a


newtype DLens cx x cy y = MkDLens {unpackLensD :: x -> (y, cy -> cx)}

view :: DLens dx x dy y -> x -> y
view (MkDLens x) = fst . x

update :: DLens dx x dy y -> x -> dy -> dx
update (MkDLens x) = snd . x

--data DFunc x y = forall cache. MkDFunc {
--    forward :: x -> (y, cache),
--    backward :: (CT y, cache) -> CT x
--  }
type DFunc x y = DLens (CT x) x (CT y) y

--type instance Tangent1 (DFunc x y) b = DFunc x (Tangent1 y b)
--type instance Dual (DFunc x y) = DFunc x (Dual y)
--type instance Tangent1 (DLens cx x cy y) b = DLens cx x (Tangent1 cy b) (Tangent1 y b)
type instance Tangent (DLens cx x cy y) = DLens cx x (Tangent cy) (Tangent y)
type instance Dual (DLens cx x cy y) = DLens cx x (Dual cy) (Dual y)


--call :: DFunc x y -> x -> y
--call (MkDFunc f _) = fst . f

--dFuncToDerivative :: DFunc a b -> (b -> CT b) -> a -> CT a
--dFuncToDerivative (MkDFunc f b) cvf = b . cross cvf id . f
pullback :: DLens cx x cy y -> (y -> cy) -> x -> cx
pullback (MkDLens l) my x = bp (my y) where
  (y, bp) = l x

identity :: DLens cx x cx x
identity = MkDLens (, id)

(%) :: DLens dy y dz z -> DLens dx x dy y -> DLens dx x dz z
(MkDLens a2) % (MkDLens a1) = MkDLens $ \x -> let
    (y, dyx) = a1 x
    (z, dzy) = a2 y
    dzx = dyx . dzy
  in (z, dzx)

--lensToDerivativePair :: DFunc a b -> a -> (b, CT b -> CT a)
--lensToDerivativePair (MkDFunc f b) x = (y, bp) where
--  bp cy = b (cy, h)
--  (y, h) = f x

--instance Category DFunc where
--  id = identity
--  (.) = (%)
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
scalarDFunc f f' = MkDLens $ \x -> (f x, \dy -> f' x * dy)
--scalarDFunc f f' = MkDFunc (fork f f') (uncurry (*))

constDLens :: (Additive ct) =>
  a -> DLens ct t ca a
constDLens c = MkDLens $ const (c, const zero)

constDFunc :: (Additive (CT t)) => 
  a -> DFunc t a
constDFunc = constDLens

-- | Optimized wrt lensCtoP
lensToUnnaryFunc :: DLens dx x dy y -> DLens dt t dx x -> DLens dt t dy y
lensToUnnaryFunc = (%)

lensToBinaryFunc :: Additive dt =>
  DLens (dx0, dx1) (x0, x1) dy y -> DLens dt t dx0 x0 -> DLens dt t dx1 x1 -> DLens dt t dy y
lensToBinaryFunc lens = curry $ lensToUnnaryFunc lens . tupleToLens


-- tuple
tupleToLens :: Additive dt =>
  (DLens dt t dx0 x0, DLens dt t dx1 x1) -> DLens dt t (dx0, dx1) (x0, x1)
tupleToLens (MkDLens d0, MkDLens d1) = MkDLens $ \t -> let
    (x0, dxt0) = d0 t
    (x1, dxt1) = d1 t
  in ((x0, x1), \(dx0, dx1) -> dxt0 dx0 + dxt1 dx1)

tripleToLens :: Additive dt =>
  (DLens dt t dx0 x0, DLens dt t dx1 x1, DLens dt t dx2 x2) -> DLens dt t (dx0, dx1, dx2) (x0, x1, x2)
tripleToLens (MkDLens a0, MkDLens a1, MkDLens a2) = MkDLens $ \t -> let
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
  DLens (ca, ca) (a, a) ca a
sumLens = MkDLens $ \(x0, x1) -> (x0 + x1, \cy -> (cy, cy))

instance (Additive x, Additive ct) =>
  Additive (DLens ct t cx x) where
    zero = MkDLens $ const (zero, zero)
    (+) = curry $ lensToUnnaryFunc sumLens . tupleToLens

subLens :: (Subtractive a, Subtractive ca) =>
  DLens (ca, ca) (a, a) ca a
subLens = MkDLens $ \(x0, x1) -> (x0 - x1, \dy -> (dy, negate dy))

instance (Subtractive y, Subtractive dy, Additive dt) => -- , Subtractive dx) =>
  Subtractive (DLens dt t dy y) where
    negate (MkDLens a) = MkDLens $ \t -> let (y, dyt) = a t in (negate y, dyt . negate)
    (-) = curry $ lensToUnnaryFunc subLens . tupleToLens


-- Mult

-- | only simple mutiplication in the same space
productLens :: (Multiplicative a, a ~ ca) =>
  DLens ct t (ca, ca) (a, a) -> DLens ct t ca a
productLens (MkDLens a) = MkDLens $ \t -> let
    ((x1, x2), dyx) = a t
  in (x1 * x2, \dy -> dyx (dy * x2, x1 * dy))

instance (Additive ct, Multiplicative a, a ~ ca) =>
  Multiplicative (DLens ct t ca a) where
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

instance (Additive ct, Divisive a, Subtractive a, CT a ~ a) =>
  Divisive (DLens ct t a a) where
    recip = lensToUnnaryFunc recipDFunc

instance (FromIntegral a b, Additive dt) =>
  FromIntegral (DLens dt t ca a) b where
    fromIntegral :: b -> DLens dt t ca a
    fromIntegral = constDLens . fromIntegral


-- Pow
--class (Ord b, Divisive a, Subtractive b, Integral b) =>
--    IntegralPower b a where
--    integralPow :: b -> a -> a
--
--instance (Ord p, Subtractive p, Integral p) =>
--  IntegralPower p Double where
--    integralPow n x = x ^^ n
--
--instance (Ord p, Subtractive p, Integral p) =>
--  IntegralPower p Float where
--    integralPow n x = x ^^ n
--
--instance (Ord p, Subtractive p, Integral p, Divisive b) =>
--  IntegralPower p (a -> b) where
--    integralPow n x = x ^^ n
--
--integralPowC :: (Divisive a, Ord p, Subtractive p, Integral p, FromIntegral a p, CT a ~ a) =>
--  p -> DFunc a a
--integralPowC n = scalarDFunc (^^ n) (\x -> fromIntegral n * (x ^^ (n - one)))
--
--instance (Additive dt, Subtractive a, IntegralPower p a, FromIntegral a p, CT a ~ a) =>
--  IntegralPower p (DLens dt t a a) where
--    integralPow n = lensToUnnaryFunc (integralPowC n)
--
--instance (Ord p, Subtractive p, Integral p) =>
--  IntegralPower p SimpleExpr where
--    integralPow n x = x ^^ n
--
--intPow :: IntegralPower Int a => Int -> a -> a
--intPow = integralPow


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
powC = MkDLens $ \(x, n) -> let xn = x ** n in (xn, \dy -> (n * (x ** (n - one)) * dy, log x * xn * dy))


instance (ExpField a, Additive ct, CT a ~ a) =>
  ExpField (DLens ct t a a) where
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
atan2C = MkDLens $ \(x, y) -> (atan2 x y, \dy -> let t = dy / (square x + square y) in (y * t, negate $ x * t))

sinhC :: (TrigField a, a ~ CT a) =>
  DFunc a a
sinhC = MkDLens $ \x -> (sinh x, \dy -> cosh x * dy)

coshC :: (TrigField a, a ~ CT a) =>
  DFunc a a
coshC = MkDLens $ \x -> (cosh x, \dy -> sinh x * dy)

asinhC :: (ExpField a, TrigField a, a ~ CT a) =>
  DFunc a a
asinhC = scalarDFunc asinh (recip . sqrt . (one +) . square)

acoshC :: (ExpField a, TrigField a, a ~ CT a) =>
  DFunc a a
acoshC = scalarDFunc asinh (recip . sqrt . (\x -> x - one) . square)

atanhC :: (TrigField a, a ~ CT a) =>
  DFunc a a
atanhC = scalarDFunc atanh (recip . (one -) . square)

instance (Additive ct, ExpField a, TrigField a, a ~ CT a) =>
  TrigField (DLens ct t a a) where
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


--class ToConst a b where
--  stopDiff :: a -> b
--
--instance ToConst a a where
--  stopDiff = id

--instance (Additive dt) =>
--  ToConst a (LensD dt t ca a) where
--    stopDiff = constC_
--
--instance (Basis a, Additive dt) =>
--  Basis (LensD dt t da a) where
--    type End (LensD dt t da a) b = End a b
--    initBackProp :: forall b. (LensD dt t da a -> b) -> End a b
--    initBackProp bp = initBackProp (bp . constC_)
--    zeroBackProp :: LensD dt t da a
--    zeroBackProp = constC_ zeroBackProp