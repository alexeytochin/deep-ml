{-# LANGUAGE TypeApplications #-}

module Numeric.InfBackprop.DLens where


import Data.Profunctor (Profunctor, dimap, Strong, first', second', Costrong, unfirst, unsecond)
import NumHask ((*), (+), sin, cos, Additive, zero, Multiplicative, one, ExpField, exp, log, (**), sqrt, sin, cos, sinh,
    asin, acos, cosh, atan, atan2, asinh, acosh, atanh, pi,
    Subtractive, negate, (-),
    Divisive, recip, (/), Field, AdditiveAction, (^), (^^), TrigField, two, Distributive, Integral, fromIntegral, FromIntegral
  )
import Prelude (fst, (.), ($), snd, const, curry, id, uncurry, Int, Ord, Float, Double, Bool, Integer, Word, undefined, Functor, fmap)
import qualified Prelude as P
import Data.Bifunctor (bimap)
import Prelude.Tools (fork)
import Optics (Iso', iso)
import Data.Functor.Classes (Show1)
import Data.Kind (Type)
import Data.Tuple.Extra (curry3)
import GHC.Natural (Natural)
import GHC.Int (Int8, Int16, Int32, Int64)
import GHC.Word (Word8, Word16, Word32, Word64)
import Debug.SimpleExpr (SimpleExpr, number)
import NumHask.AdvancedActions (LeftRightMultiplicativeAction, (.*), LeftMultiplicativeAction)
import Control.PolyFunctor (Vectorizable, ffmap, Vectorizable1, ffmap1, binarryFfmap1, BinnaryVectorizable1)
-- import Numeric.InfBackprop.Cotangent (Dual, CT, HasBasis, basis, T1, Tangent1, zeroB, basis1, basis)
import Data.Data (Proxy(Proxy))
import GHC.TypeNats (Nat)
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector as DV
import qualified Data.Vector.Fixed.Boxed as DVFB
import qualified Data.Vector.Fixed as DVF


type Vec (n :: Nat) a = DVGS.Vector DV.Vector n a

--type family Dual (x :: Type) :: Type
--type instance Dual Float = Float
--type instance Dual (a, b) = (Dual a, Dual b)
--type instance Dual (a, b, c) = (Dual a, Dual b, Dual c)
--type instance Dual [a] = [Dual a]
--type instance Dual (DVFB.Vec n a) = DVFB.Vec n (Dual a)
--type instance Dual (LongVec n a) = LongVec n (Dual a)
--type instance Dual (Stream a) = BoundedStream (Dual a)
--type instance Dual (BoundedStream a) = Stream (Dual a)

--type family GetField (a :: Type) :: Type
--type instance GetField Float = Float

type family Conangent (a :: Type) :: Type
type instance Conangent Float = Float
type instance Conangent (DLens ca a cb b) = Float



type family Basis1 (a :: Type) (b :: Type) :: Type
type instance Basis1 Float c = Conangent c
type instance Basis1 (DLens ca a cb b) c = DLens ca a (Basis1 cb c) (Basis1 b c)
type instance Basis1 (Vec n a) c = Vec n (Basis1 a c) -- Tangent1 (LongVec n a) (CT (LongVec n a))

--type Basis a = Basis1 a a
--
--
--type family Tangent1 (a :: Type) :: Type
--type instance Tangent1 (DLens da a db b) c = DLens da a (Tangent1 db c) (Tangent1 b c)
-- type instance Dual (DLens da a db b) = DLens da a (Dual db) (Dual b)


-- | We define a differentiable function as a law breaking lens.
newtype DLens cx x cy y = DLens {unpackDLens :: x -> (y, cy -> cx)}

-- type instance Tangent (LensD dx x dy y) = LensD dx x (T dy) (T y)

view :: DLens cx x cy y -> x -> y
view (DLens x) = fst . x

update :: DLens cx x cy y -> x -> cy -> cx
update (DLens x) = snd . x

pullback :: DLens cx x cy y -> (y -> cy) -> x -> cx
pullback (DLens l) my x = bp (my y) where
  (y, bp) = l x

identity :: DLens cx x cx x
identity = DLens (, id)

(%) :: DLens cy y cz z -> DLens cx x cy y -> DLens cx x cz z
(DLens a2) % (DLens a1) = DLens $ \x -> let
    (y, dyx) = a1 x
    (z, dzy) = a2 y
    dzx = dyx . dzy
  in (z, dzx)



--instance (Basis a, Additive dt) =>
--  Basis (LensD dt t da a) where
--    type End (LensD dt t da a) b = End a b -- (LensD dt t (T b) b)
--    --    basis   :: forall b. (a -> b) -> End a b
--    initBackProp :: forall b. (LensD dt t da a -> b) -> End a b
--    initBackProp bp = initBackProp (bp . constC_)
--    zeroBackProp :: LensD dt t da a
--    zeroBackProp = constC_ zeroBackProp

constC :: forall a b ca cb. (Additive ca) =>
  b ->
  DLens ca a cb b
constC c = DLens $ const (c, const zero)

--constC' :: forall a b ca proxy. (Additive (Tangent1 a ca)) =>
--  proxy ca -> -- () =>
--  b ->
--  DLens (T1 a ca) a (T1 b ca) b
--constC' _ c = DLens $ const (c, const zero)

--instance (HasBasis a, Additive ct) =>
--  HasBasis (DLens ct t ca a) where
--    --  basis1 :: forall b proxy. proxy a -> proxy b -> (CT a -> CT b) -> T1 a (CT b)
--    basis1 :: forall b proxy.
--      proxy (DLens ct t ca a) ->
--      proxy b ->
--      (DLens ct t (CT ca) (CT a) -> CT b) ->
--      DLens ct t (T1 ca (CT b)) (T1 a (CT b))
--    -- (%) :: DLens cy y (T1 cb (CT c)) (T1 b (CT c)) -> DLens ca a cy y -> DLens ca a (T1 cb (CT c)) (T1 b (CT c))
--    -- (%) :: DLens (CT cb) (CT b) (T1 cb (CT c)) (T1 b (CT c)) -> DLens ca a (CT cb) (CT b) -> DLens ca a (T1 cb (CT c)) (T1 b (CT c))
--    basis1 _ _ f = constC $ basis1 (Proxy @a) (Proxy @b) (f . constC)
----      (Proxy @(CT b))
----      undefined -- (basis1 (Proxy @a) (Proxy @b) undefined :: T1 a (CT b))
--      -- constLens = constC (Proxy @(CT b)) (basis1 (Proxy @a) (Proxy @b) undefined :: T1 a (CT b))
----     f = DLens $ \x -> let
----        y = undefined
----      in (y :: (T1 b (CT c)), undefined)
----      (sbpLens :: DLens cb b (T1 cb (CT c)) (T1 b (CT c))) % lens where
----      sbpLens = DLens $ \b -> let
----          tbctc = basis1 (Proxy @c) (f . constC) b :: T1 b (CT c)
----          bp = undefined
----        in (tbctc :: T1 b (CT c), bp :: T1 cb (CT c) -> cb)
--
----      basis1 _ c (DLens l) = DLens $ \x -> let
----        (y, bp) = l x
----        in undefined
--
--    -- zeroB :: proxy a -> CT a
--    zeroB :: proxy (DLens ct t ca a) -> DLens ct t (CT ca) (CT a)
--    zeroB _ = constC $ zeroB (Proxy @a)


derivativeTuple ::
  (DLens cx x cx x -> DLens cx x cy y) ->
  x ->
  (y, cy -> cx)
derivativeTuple f = unpackDLens (f identity)

derivativeOp ::
  (DLens cx x cx x -> DLens cx x cy y) ->
  (y -> cy) ->
  x ->
  cx
derivativeOp f sb x = dyx (sb y) where
  (y, dyx) = derivativeTuple f x

--type DLens1 t a b = DLens (T1 t (CT a)) a (T1 t (CT b)) b
--
--type DFunc ca a cb b = DLens ca a ca a -> DLens ca a cb b
--type DFunc1 t a b ct ca cb = DLens ct t ca a -> DLens ct t cb b
--type DFunc2 t a b = DFunc (T1 t (CT a)) a (T1 t (CT b)) b


--derivative :: forall a b. (HasBasis b) =>
--  --(DLens (CT a) a (CT a) a -> DLens (CT a) a (CT b) b) ->
--  (DLens (T1 b (CT a)) a (T1 b (CT a)) a -> DLens (T1 b (CT a)) a (T1 b (CT b)) b) ->
--  -- DFunc (T1 b (CT a)) a (T1 b (CT b)) b -> 
--  -- (DLens1 b a a -> DLens1 b a b) ->
--  a ->
--  T1 b (CT a)
--derivative f = derivativeOp f (const (basis (Proxy @b)) :: b -> T1 b (CT b))





--class HasFuncBasis a where
--  type FuncBasis (a :: Type) (b :: Type) :: Type
--  basisOne :: forall b. (a -> b) -> FuncBasis a b
--  basisZero :: a
--
--instance HasFuncBasis Float where
--  type FuncBasis Float b = b
--  basisOne f = f 1
--  basisZero = 0
--
--instance forall a1 a2. (HasFuncBasis a1, HasFuncBasis a2) =>
--  HasFuncBasis (a1, a2) where
--    type FuncBasis (a1, a2) b = (FuncBasis a1 b, FuncBasis a2 b)
--    basisOne :: ((a1, a2) -> b) -> (FuncBasis a1 b, FuncBasis a2 b)
--    basisOne bp = (
--        basisOne (\a1 -> bp (a1, basisZero)),
--        basisOne (\a2 -> bp (basisZero, a2))
--      )
--    basisZero :: (a1, a2)
--    basisZero = (basisZero, basisZero)



--derivative :: HasFuncBasis cy =>
--  (DLens cx x cx x -> DLens cx x cy y) ->
--  x ->
--  FuncBasis cy cx
--derivative f x = funcBasis (snd (derivativeOp f x))


lensToUnnaryFunc :: DLens dx x dy y -> DLens dt t dx x -> DLens dt t dy y
lensToUnnaryFunc = (%)





--instance (Temp (DLens a b c d), Functor f) =>
--  Temp (DLens (f a) b (f c) d)





--mkScalarDFunc :: (Multiplicative x, Functor f) =>
--  (x -> x) -> (x -> x) -> DLens (f x) x (f x) x
--mkScalarDFunc f f' = DLens $ \x -> (f x, fmap (f' x *))

mkScalarDFuncV2 :: (
    Multiplicative x, 
    Vectorizable1 x cx
--    Vectorizable x x cx cx
  ) =>
  (x -> x) -> (x -> x) -> DLens cx x cx x
mkScalarDFuncV2 f f' = DLens $ \x -> (f x, ffmap1 (f' x *))

sinC :: (TrigField a, Vectorizable1 a ca) => -- Vectorizable a a ca ca ) =>
  DLens ca a ca a
--sinC = DLens $ \x -> (sin x, fmap (cos x *))
sinC = mkScalarDFuncV2 sin cos

--sinC :: (TrigField x) =>
--  DLens cx x cx x
--sinC = mkDFunc sin cos

instance (
    -- Additive ct, 
    BinnaryVectorizable1 t ct,
    Additive x
  ) =>
    Additive (DLens ct t cx x) where
      DLens l0 + DLens l1 = DLens $ \t -> let
          (x0, bp0) = l0 t
          (x1, bp1) = l1 t
        in (x0 + x1, \cy -> binarryFfmap1 (+) (bp0 cy) (bp1 cy))
      zero = DLens $ const (zero, zero)

instance (
    -- Subtractive ct, 
    Subtractive a, 
    Subtractive ca
  ) =>
    Subtractive (DLens ct t ca a) where
      DLens l0 - DLens l1 = DLens $ \t -> let
          (x0, bp0) = l0 t
          (x1, bp1) = l1 t
        in (x0 - x1, \cy -> bp0 cy + bp1 (negate cy))
      negate (DLens l) = DLens $ \t -> let
          (x, bp) = l t
        in (negate x, bp . negate)

instance (
    Multiplicative a, 
    Additive ct, 
    Vectorizable1 a ca
--    Vectorizable a a ca ca
  ) => -- LeftMultiplicativeAction a ca) =>
    Multiplicative (DLens ct t ca a) where
      DLens l0 * DLens l1 = DLens $ \t -> let
          (x0, bp0) = l0 t
          (x1, bp1) = l1 t
        in (x0 * x1, \cy -> bp0 (ffmap1 (x1 *) cy) + bp1 (ffmap1 (x0 *) cy))
      one = DLens $ const (one, zero)

instance (
    Divisive a, 
    Vectorizable1 a ca, 
    --Vectorizable a a ca ca, 
    Subtractive a, 
    Additive ct
  ) =>
    Divisive (DLens ct t ca a) where
      DLens l0 / DLens l1 = DLens $ \t -> let
          (x0, bp0) = l0 t
          (x1, bp1) = l1 t
        in (x0 / x1, \cy -> bp0 (ffmap1 (recip x1 *) cy) + bp1 (ffmap1 ((negate $ recip (x1 * x1)) *) cy))
      recip (DLens l) = DLens $ \t -> let
          (x, bp) = l t
        in (recip x, bp . ffmap1 ((negate $ recip (x * x)) *))

instance (
    -- Subtractive ca,
    -- Subtractive ct,
    --Vectorizable a a ca ca,
    Subtractive a,
    Vectorizable1 a ca,
--    Vectorizable a a ca ca,
    TrigField a
  ) =>
    TrigField (DLens ct t ca a) where
      sin = undefined -- lensToUnnaryFunc $ mkScalarDFuncV2 sin cos
      cos = undefined -- lensToUnnaryFunc $ mkScalarDFuncV2 cos (negate . sin)
      asin = undefined
      acos = undefined
      atan = undefined
      atan2 = undefined
      sinh = undefined -- lensToUnnaryFunc $ mkScalarDFuncV2 cosh sinh
      cosh = undefined -- lensToUnnaryFunc $ mkScalarDFuncV2 sinh cosh
      asinh = undefined
      acosh = undefined
      atanh = undefined
      pi = undefined


-- Examples
example_0_0 = cos (0.0 :: Float) :: Float
-- example_0_1_ :: TrigField a => a -> Float -> a
-- derivativeOp :: (DFunc x x -> DFunc x y) -> (y -> T y) -> x -> T x
example_0_1 = derivativeOp cos :: (Float -> Float) -> Float -> Float -- :: Float
example_0_2 = derivativeOp cos (const 1 :: (Float -> Float)) :: Float -> Float -- :: Float
example_0_3 = derivativeOp cos (const one :: Multiplicative a => (a -> a)) :: Float -> Float -- :: Float
example_0_4 = derivativeOp cos (const one :: Multiplicative a => (a -> a)) (0 :: Float)