{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

module InfBackprop.LensD where

import Data.Profunctor (Profunctor, dimap, Strong, first', second', Costrong, unfirst, unsecond)
import NumHask ((*), (+), sin, cos, Additive, zero, Multiplicative, one, ExpField, exp, log, (**), sqrt, sin, cos, sinh,
    asin, acos, cosh, atan, atan2, asinh, acosh, atanh, pi,
    Subtractive, negate, (-),
    Divisive, recip, (/), Field, AdditiveAction, (^), (^^), TrigField, two, Distributive, Integral, fromIntegral, FromIntegral
  )
import Prelude (fst, (.), ($), snd, const, curry, id, uncurry, Int, Ord, Float, Double, Bool, Integer, Word)
import qualified Prelude as P
import Data.Bifunctor (bimap)
import Prelude.Tools (fork)
import InfBackprop.Tangent (Tangent, T)
import Data.Basis3 (Basis, initBackProp , zeroBackProp, End)
import Optics (Iso', iso)
import Data.Functor.Classes (Show1)
import Data.Kind (Type)
import Data.Tuple.Extra (curry3)
import GHC.Natural (Natural)
import GHC.Int (Int8, Int16, Int32, Int64)
import GHC.Word (Word8, Word16, Word32, Word64)
import Debug.SimpleExpr (SimpleExpr, number)



--data D dx x dy y  = D {
--  view    :: x -> y,
--  update  :: x -> dy -> dx
--}

-- | We define a differentiable function as a law breaking lens.
newtype LensD dx x dy y = LensD {unpackLensD :: x -> (y, dy -> dx)}

type instance Tangent (LensD dx x dy y) = LensD dx x (T dy) (T y)

view :: LensD dx x dy y -> x -> y
view (LensD x) = fst . x

update :: LensD dx x dy y -> x -> dy -> dx
update (LensD x) = snd . x

pullback :: LensD cx x cy y -> (y -> cy) -> x -> cx
pullback (LensD l) my x = bp (my y) where
  (y, bp) = l x

identity :: LensD dx x dx x
identity = LensD (, id)

(%) :: LensD dy y dz z -> LensD dx x dy y -> LensD dx x dz z
(LensD a2) % (LensD a1) = LensD $ \x -> let
    (y, dyx) = a1 x
    (z, dzy) = a2 y
    dzx = dyx . dzy
  in (z, dzx)

instance Profunctor (LensD dt t) where
  dimap :: (dz -> dy) -> (y -> z) -> LensD dx x dy y -> LensD dx x dz z
  dimap f g (LensD a) = LensD $ \x -> let
      (y, dyx) = a x
      y' = g y
      dyx' = dyx . f
    in (y', dyx')

instance Costrong (LensD dt t) where
  -- unfirst  :: p (a, d) (b, d) -> p a b
  unfirst :: LensD dt t (dx, s) (x, s) -> LensD dt t dx x
  -- unfirst (D v u) = D (fst . v) (\t dx -> u t (dx, snd (v t)))
  unfirst (LensD a) = LensD $ \x -> let
      ((y, s), dyx) = a x
      dyx' dx = dyx (dx, s)
    in (y, dyx')

type DP dx x dy y = forall p. Costrong p => p dx x -> p dy y

lensCtoP :: LensD dx x dy y -> DP dx x dy y
lensCtoP l = unsecond . dimap (uncurry u) (fork id v) where
  v = view l
  u = update l

lensPtoC :: DP dx x dy y -> LensD dx x dy y
lensPtoC f = f identity

--tupleToLens' :: Additive dt =>
--  (LensD dt t dx1 x1, LensD dt t dx2 x2) -> LensD dt t (dx1, dx2) (x1, x2)
--tupleToLens' (LensD a1, LensD a2) = LensD $ \t -> let
--    (x1, dxt1) = a1 t
--    (x2, dxt2) = a2 t
--  in ((x1, x2), \(dx1, dx2) -> dxt1 dx1 + dxt2 dx2)




-- Typeclass attempts
type X = Show1

--class Lensable f g | f -> g where
--  toLens :: forall dt t dx x. (Additive dt) =>
--    f (LensD dt t dx x) -> LensD dt t (g dx) (f x)
--  fromLens :: forall dt t dx x. (Additive dt, Additive dx) =>
--    LensD dt t (g dx) (f x) -> f (LensD dt t dx x)
--  iso' :: (Additive dt, Additive dx) => 
--    Iso' (f (LensD dt t dx x)) (LensD dt t (g dx) (f x)) 
--  iso' = iso toLens fromLens 

--class Lensable' a dt t where
--  -- type LensableType dt t dx x :: Type
--  lensIso' :: (Additive dt) => 
--    Iso' (LensableType dt t dx x) (LensD dt t dx x)




class Lensable dx x where
  type LensableType dt t dx x :: Type
  lensIso :: (Additive dt) => 
    Iso' (LensableType dt t dx x) (LensD dt t dx x)

--instance Lensable (LensD dt t dx x) dx x 

class IsoMap f where
  type F f b :: Type
  liftIso :: Iso' a b -> Iso' (f a) ((F f) b)

class Lensable1 f df | f -> df where
  liftIso1 :: 
    (forall dt' t'. (Additive dt') => Iso' (l dt' t' dx x) (LensD dt' t' dx x)) -> 
    (forall dt t. (Additive dt) => Iso' (f (l dt t dx x)) (LensD dt t (df dx) (f x)))


--binnaryFuncToLens :: LensD dt t dx1 x1 -> LensD dt t dx2 x2 -> LensD dt t (dx1, dx2) (x1, x2)


idIso :: Iso' (LensD dt t da a) (LensD dt t da a)
idIso = iso id id

-- | Optimized wrt lensCtoP
lensToUnnaryFunc :: LensD dx x dy y -> LensD dt t dx x -> LensD dt t dy y
lensToUnnaryFunc = (%)
--lensToFunc (LensD lxy) (LensD ltx) = LensD $ \t -> let
--    (y, dxy) = lxy x
--    (x, dxy) = ltx t
--  in ()


-- tuple
tupleToLens :: Additive dt =>
  (LensD dt t dx1 x1, LensD dt t dx2 x2) -> LensD dt t (dx1, dx2) (x1, x2)
-- tupleToLens (D v1 u1, D v2 u2) = D (\t -> (v1 t, v2 t)) (\t (dy1, dy2) -> u1 t dy1 + u2 t dy2)
tupleToLens (LensD a1, LensD a2) = LensD $ \t -> let
    (x1, dxt1) = a1 t
    (x2, dxt2) = a2 t
  in ((x1, x2), \(dx1, dx2) -> dxt1 dx1 + dxt2 dx2)
-- crossC3 (D v1 u1) (D v2 u2) = D (fork v1 v2) ()

--tupleToLens_ :: Additive (T t) =>
--  (DFunc t x1, DFunc t x2) -> DFunc t (x1, x2)
--tupleToLens_ = tupleToLens

--tupleValue :: Additive (T t) =>
--  (x -> (DFunc t x1, DFunc t x2)) -> x -> DFunc t (x1, x2)
--tupleValue f = tupleToLens_ . f

lensToTuple :: (Additive dx0, Additive dx1) =>
  LensD dt t (dx0, dx1) (x0, x1) -> (LensD dt t dx0 x0, LensD dt t dx1 x1)
lensToTuple (LensD a) = (LensD a0, LensD a1) where
  a0 = \t -> let
      ((x0, _), dxt) = a t
    in (x0, \dx0 -> dxt (dx0, zero))
  a1 = \t -> let
      ((_, x1), dxt) = a t
    in (x1, \dx1 -> dxt (zero, dx1))

tupleLensIso :: (Additive da0, Additive da1, Additive dt) =>
  Iso' (LensD dt t (da0, da1) (a0_, a1_)) (LensD dt t da0 a0_, LensD dt t da1 a1_)
tupleLensIso = iso lensToTuple tupleToLens

instance (Additive dx0, Additive dx1) => 
  Lensable (dx0, dx1) (x0, x1) where
    type LensableType dt t (dx0, dx1) (x0, x1) = (LensD dt t dx0 x0, LensD dt t dx1 x1)
    lensIso = iso tupleToLens lensToTuple 

lensToBinaryFunc :: Additive dt =>
  LensD (dx0, dx1) (x0, x1) dy y -> LensD dt t dx0 x0 -> LensD dt t dx1 x1 -> LensD dt t dy y
lensToBinaryFunc lens = curry $ lensToUnnaryFunc lens . tupleToLens


-- Triple
tripleToLens :: Additive dt =>
  (LensD dt t dx0 x0, LensD dt t dx1 x1, LensD dt t dx2 x2) -> LensD dt t (dx0, dx1, dx2) (x0, x1, x2)
tripleToLens (LensD a0, LensD a1, LensD a2) = LensD $ \t -> let
    (x0, dxt0) = a0 t
    (x1, dxt1) = a1 t
    (x2, dxt2) = a2 t
  in ((x0, x1, x2), \(dx0, dx1, dx2) -> dxt0 dx0 + dxt1 dx1 + dxt2 dx2)

lensToTriple :: (Additive dx0, Additive dx1, Additive dx2) =>
  LensD dt t (dx0, dx1, dx2) (x0, x1, x2) -> (LensD dt t dx0 x0, LensD dt t dx1 x1, LensD dt t dx2 x2)
lensToTriple (LensD a) = (LensD a0, LensD a1, LensD a2) where
  a0 = \t -> let
      ((x0, _, _), dxt) = a t
    in (x0, \dx0 -> dxt (dx0, zero, zero))
  a1 = \t -> let
      ((_, x1, _), dxt) = a t
    in (x1, \dx1 -> dxt (zero, dx1, zero))
  a2 = \t -> let
      ((_, _, x2), dxt) = a t
    in (x2, \dx2 -> dxt (zero, zero, dx2))

tripleLensIso' :: (Additive da0, Additive da1, Additive da2, Additive dt) =>
  Iso' (LensD dt t (da0, da1, da2) (a0, a1, a2)) (LensD dt t da0 a0, LensD dt t da1 a1, LensD dt t da2 a2)
tripleLensIso' = iso lensToTriple tripleToLens

lensToTernaryFunc :: forall dx0 dx1 dx2 x0 x1 x2 dy y dt t. Additive dt =>
  LensD (dx0, dx1, dx2) (x0, x1, x2) dy y -> LensD dt t dx0 x0 -> LensD dt t dx1 x1 -> LensD dt t dx2 x2 -> LensD dt t dy y
lensToTernaryFunc lens = curry3 $ (lensToUnnaryFunc lens :: LensD dt t (dx0, dx1, dx2) (x0, x1, x2) -> LensD dt t dy y) . tripleToLens



--mapToLens :: forall r dt t dx x. Additive dt =>
--  (r -> LensD dt t dx x) -> LensD dt t (r -> dx) (r -> x)
--mapToLens lensMap = LensD $ \t -> let
--    xr = \r -> fst $ unpackLensD (lensMap r) t
--    dxtr = \r -> snd $ unpackLensD (lensMap r) t :: (r -> dx) -> dt
--  in (xr :: r -> x, dxtr :: (r -> dx) -> dt)




-- Const
constLens :: (Additive dx) =>
  y -> LensD dx x dy y
constLens c = LensD (const (c, const zero))


-- Sum
sumLens :: (Additive x) =>
  LensD (dx, dx) (x, x) dx x
sumLens = LensD $ \(x1, x2) -> (x1 + x2, \dy -> (dy, dy))

--sumLens :: (Additive x) =>
--  LensD dt t (dx, dx) (x, x) -> LensD dt t dx x
--sumLens (LensD a) = LensD $ \t -> let
--    ((x1, x2), dyx) = a t
--  in (x1 + x2, \dy -> dyx (dy, dy))

instance (Additive x, Additive dt) =>
  Additive (LensD dt t dx x) where
    zero = LensD $ const (zero, zero)
    (+) = curry $ lensToUnnaryFunc sumLens . tupleToLens
--    (LensD a1) + (LensD a2) = LensD $ \x -> let
--        (y1, dyx1) = a1 x
--        (y2, dyx2) = a2 x
--      in (y1 + y2, dyx1 + dyx2)

subLens :: (Subtractive x, Subtractive dx) =>
  LensD (dx, dx) (x, x) dx x
subLens = LensD $ \(x1, x2) -> (x1 - x2, \dy -> (dy, negate dy))

instance (Subtractive y, Subtractive dy, Additive dt) => -- , Subtractive dx) =>
  Subtractive (LensD dt t dy y) where
    negate (LensD a) = LensD (\t -> let (y, dyt) = a t in (negate y, dyt . negate))
    (-) = curry $ lensToUnnaryFunc subLens . tupleToLens
--    LensD a1 - LensD a2 = LensD $ \t -> let
--        (y1, dyt1) = a1 t
--        (y2, dyt2) = a2 t
--      in (y1 - y2, \dy -> dyt1 dy + dyt2 (negate dy))


-- DFunc
type DFunc x y = LensD (T x) x (T y) y

constC :: (Additive (T x)) =>
  y -> DFunc x y
constC c = LensD (const (c, const zero))

constC_ :: (Additive dx) =>
  y -> LensD dx x dy y
constC_ c = LensD (const (c, const zero))

mkDFunc :: (Multiplicative x, x ~ T x) =>
  (x -> x) -> (x -> x) -> DFunc x x
mkDFunc f f' = LensD $ \x -> (f x, \dy -> f' x * dy)

-- Mult

-- | only simple mutiplication in the same space
productLens :: (Multiplicative x, x ~ dx) =>
  LensD dt t (dx, dx) (x, x) -> LensD dt t dx x
productLens (LensD a) = LensD $ \t -> let
    ((x1, x2), dyx) = a t
  in (x1 * x2, \dy -> dyx (dy * x2, x1 * dy))

instance (Additive dt, Multiplicative y, y ~ dy) => -- instance (Additive dx, ) => --
  Multiplicative (LensD dt t dy y) where
    one = constLens one
    (*) = curry $ productLens . tupleToLens
--    LensD a1 * LensD a2 = LensD $ \x -> let
--        (y1, dyx1) = a1 x
--        (y2, dyx2) = a2 x
--      in (y1 * y2, \dy -> y2 * dyx1 dy + y1 * dyx2 dy)



#if MIN_VERSION_numhask(0,11,0)
#else
instance (Additive dx, Distributive dy, y ~ dy) =>
  Distributive (LensD dx x dy y)
#endif

--squareC :: (Additive x, Multiplicative x, T x ~ x) => -- , Diff x ~ x) =>
--  DFunc x x
--squareC = LensD $ \x -> (x * x, \dy -> two * x * dy)
--
----square :: (Additive x, Multiplicative x, T x ~ x) =>
----  DP x x x x
----square = lensCtoP squareC
--
--recipC :: (Divisive x, Subtractive x, T x ~ x) =>
--  LensD x x x x
--recipC = LensD $ \x -> (recip x, \dy -> negate $ recip x^2 * dy)
--
--instance (Divisive x, Subtractive dx, dx ~ x, T x ~ dx) =>
--  Divisive (LensD dx x dx x) where
--    recip = lensCtoP recipC
--
--instance (Field x) => 
--  Field (LensD x x x x)


-- Divisible
square :: Multiplicative x => x -> x
square x = x * x

squareC :: (Additive x, Multiplicative x, T x ~ x) => -- , Diff x ~ x) =>
  DFunc x x
squareC = mkDFunc square (two *)
-- squareC = LensD $ \x -> (x * x, \dy -> two * x * dy)

--square :: (Additive x, Multiplicative x, T x ~ x) =>
--  DP x x x x
--square = lensCtoP squareC

recipC :: (Divisive x, Subtractive x, T x ~ x) =>
  DFunc x x
recipC = mkDFunc recip (\x -> negate $ recip x^2)
-- recipC = LensD $ \x -> (recip x, \dy -> negate $ recip x^2 * dy)

--instance (Distributive dx, y ~ dx) => 
--  Divisive (LensD dx x dy y) where
--    recip = lensCtoP recipC

instance (Additive dt, Divisive x, Subtractive dx, dx ~ x, T x ~ dx) => -- , dt ~ T t
  Divisive (LensD dt t dx x) where
    recip = lensToUnnaryFunc recipC -- :: forall dt t. LensD dt t dx x -> LensD dt t dx x

instance (FromIntegral a b, Additive dt) =>
  FromIntegral (LensD dt t da a) b where
    fromIntegral :: b -> LensD dt t da a
    fromIntegral = constC_ . fromIntegral


-- Pow
class (Ord b, Divisive a, Subtractive b, Integral b) => 
  IntegralPower b a where
    integralPow :: b -> a -> a 

instance (Ord p, Subtractive p, Integral p) => 
  IntegralPower p Double where
    integralPow n x = x ^^ n

instance (Ord p, Subtractive p, Integral p) => 
  IntegralPower p Float where
    integralPow n x = x ^^ n

instance (Ord p, Subtractive p, Integral p, Divisive b) => 
  IntegralPower p (a -> b) where
    integralPow n x = x ^^ n


integralPowC :: (Divisive a, Ord p, Subtractive p, Integral p, FromIntegral a p, T a ~ a) =>
  p -> DFunc a a
integralPowC n = mkDFunc (^^ n) (\x -> fromIntegral n * (x ^^ (n - one)))

--pow' :: (Divisive a, Ord b, Subtractive b, Integral b, FromIntegral a b, T a ~ a) =>
--  b -> LensD dt t (T a) a -> LensD dt t (T a) a
--pow' = lensToUnnaryFunc . integralPowC

instance (Additive dt, Subtractive a, IntegralPower p a, FromIntegral a p, T a ~ a) => 
  IntegralPower p (LensD dt t a a) where
    integralPow n = lensToUnnaryFunc (integralPowC n)

instance (Ord p, Subtractive p, Integral p) =>
  IntegralPower p SimpleExpr where
    integralPow n x = x ^^ n

intPow :: IntegralPower Int a => Int -> a -> a
intPow = integralPow


---- SimpleExprExtra
--instance FromIntegral Integer n => FromIntegral SimpleExpr n  where
--  fromIntegral n = number (fromIntegral n)


-- Field
#if MIN_VERSION_numhask(0,11,0)
#else
instance (Additive dt, Field x, x ~ T x) => -- , dt ~ T t) => 
  Field (LensD dt t x x)
#endif

sqrtC :: (ExpField x, x ~ T x) => DFunc x x
sqrtC = mkDFunc sqrt (recip . (two *) . sqrt)
--sqrtC = LensD $ \x -> (sqrt x, \dy -> recip (two * sqrt x) * dy)

expC :: (ExpField x, x ~ T x) => DFunc x x
expC = mkDFunc exp exp

-- | wrong for negative x
logC :: (ExpField x, x ~ T x) =>
  DFunc x x
logC = mkDFunc log recip

powC :: (ExpField x, x ~ T x) =>
  DFunc (x, x) x
powC = LensD $ \(x, n) -> let xn = x ** n in (xn, \dy -> (n * (x ** (n - one)) * dy, log x * xn * dy))


instance (ExpField x, Additive dt, T x ~ x) =>
  ExpField (LensD dt t x x) where
    exp = lensToUnnaryFunc expC
    log = lensToUnnaryFunc logC
    (**) = lensToBinaryFunc powC
    sqrt = lensToUnnaryFunc sqrtC

sinC :: (TrigField x, x ~ T x) =>
  DFunc x x
sinC = mkDFunc sin cos

cosC :: (TrigField x, x ~ T x) =>
  DFunc x x
cosC = mkDFunc cos (negate . sin)

asinC :: (ExpField x, TrigField x, x ~ T x) =>
  DFunc x x
asinC = mkDFunc asin (recip . sqrt . (one -) . square)

-- | Backprop lens version of arccos.
acosC :: (ExpField x, TrigField x, x ~ T x) =>
  DFunc x x
acosC = mkDFunc acos (negate . recip . sqrt . (one -) . square)

atanC :: (TrigField x, x ~ T x) =>
  DFunc x x
atanC = mkDFunc atan (recip . (one +) . square)

atan2C :: (TrigField a, a ~ T a) =>
  DFunc (a, a) a
atan2C = LensD $ \(x, y) -> (atan2 x y, \dy -> let t = dy / (square x + square y) in (y * t, negate $ x * t))

sinhC :: (TrigField x, x ~ T x) =>
  DFunc x x
sinhC = LensD $ \x -> (sinh x, \dy -> cosh x * dy)

coshC :: (TrigField x, x ~ T x) =>
  DFunc x x
coshC = LensD $ \x -> (cosh x, \dy -> sinh x * dy)

asinhC :: (ExpField x, TrigField x, x ~ T x) =>
  DFunc x x
asinhC = mkDFunc asinh (recip . sqrt . (one +) . square)

acoshC :: (ExpField x, TrigField x, x ~ T x) =>
  DFunc x x
acoshC = mkDFunc asinh (recip . sqrt . (\x -> x - one) . square)

atanhC :: (TrigField x, x ~ T x) =>
  DFunc x x
atanhC = mkDFunc atanh (recip . (one -) . square)

instance (Additive dt, ExpField x, TrigField x, x ~ T x, dt ~ T t) =>
  TrigField (LensD dt t x x) where
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
    pi = constLens pi


class ToConst a b where
  stopDiff :: a -> b

instance ToConst a a where
  stopDiff = id

instance (Additive dt) =>
  ToConst a (LensD dt t da a) where
    stopDiff = constC_

instance (Basis a, Additive dt) =>
  Basis (LensD dt t da a) where
    type End (LensD dt t da a) b = End a b
    initBackProp :: forall b. (LensD dt t da a -> b) -> End a b
    initBackProp bp = initBackProp (bp . constC_)
    zeroBackProp :: LensD dt t da a
    zeroBackProp = constC_ zeroBackProp