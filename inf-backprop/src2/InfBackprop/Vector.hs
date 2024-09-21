{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module InfBackprop.Vector where

import InfBackprop.LensD (LensD(LensD), unpackLensD, Lensable, LensableType, lensIso)
import NumHask (Additive, sum, zero, (+), Distributive, (-), one)
import GHC.TypeLits (KnownNat, Nat, type (+))
import Prelude (($), fmap, (<*>))
import Prelude hiding (sinh, cosh, (*), (+), (-), negate, recip, exp, (^), (^^), (/), log, sqrt, unzip, sum)
import Data.List.NonEmpty (unzip)
--import Data.Vector.Generic.Sized (index, generate)
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector as DV
--import Data.Vector.Fixed.Boxed (Vec)
import qualified Data.Vector.Fixed.Boxed as DVFB
import qualified Data.Vector.Fixed as DVF
import Data.Finite.Internal (Finite(Finite), getFinite)
import Data.Vector.Fixed.Cont (ArityPeano, Peano, PeanoNum(S), vector, ContVec, apply)
import InfBackprop.Tangent (T, Tangent)
import Optics (iso, Iso')
import GHC.Natural (Natural)
import Data.Basis3 (Basis, End, initBackProp, zeroBackProp)
--import Data.HashMap.Internal.Array (Array)
import Data.Foldable (foldl')
import Control.Applicative (Const(Const))

--type X = Array

type LongVec (n :: Nat) a = DVGS.Vector DV.Vector n a
type SmallVec (n :: Nat) a = DVFB.Vec n a

--instance (KnownNat n, Additive a) => 
--  Additive (LongVec n a) where
--    zero = DVGS.replicate zero
--    (+) = DVGS.zipWith (+)
--
--instance (DVF.Arity n, Additive a) => 
--  Additive (SmallVec n a) where
--    zero = DVF.replicate zero
--    (+) = DVF.zipWith (+)

longVecSum :: Additive a => LongVec n a -> a
longVecSum = foldl' (+) zero

vecToLens :: forall dt t dx x vec (n :: Nat). (Additive dt, Applicative (vec n), Foldable (vec n)) =>
  vec n (LensD dt t dx x) -> LensD dt t (vec n dx) (vec n x)
vecToLens a = LensD $ \t ->
  let (vec, dVecT) = unzip (fmap (`unpackLensD` t) a)
  in (vec, \dVec -> sum (dVecT <*> dVec))

-- |
-- >>> import Prelude (Int, (*))
-- >>> fv = DVGS.fromTuple (\x -> x, \x -> 2 * x, \x -> 3 * x) :: LongVec 3 (Int -> Int)
-- >>> v = DVGS.fromTuple (1, 2, 2) :: LongVec 3 Int
-- >>> sum $ fv <*> v 
-- 11
longVecToLens :: forall dt t dx x n. (Additive dt, KnownNat n) =>
  LongVec n (LensD dt t dx x) -> LensD dt t (LongVec n dx) (LongVec n x)
longVecToLens a = LensD $ \t ->
  let (vec, dVecT) = DVGS.unzip (fmap (`unpackLensD` t) a) :: (LongVec n x, LongVec n (dx -> dt)) --
  in (vec, \dVec -> longVecSum (dVecT <*> dVec))

smallVecToLens :: (Additive dt, DVF.Arity n) =>
  SmallVec n (LensD dt t dx x) -> LensD dt t (SmallVec n dx) (SmallVec n x)
smallVecToLens = vecToLens


basisLongVec :: (KnownNat n) =>
  Finite n -> a -> a -> LongVec n a
basisLongVec k zero' one' = DVGS.generate $ \l ->
  if k == l
    then one'
    else zero'

basisSmallVec :: (DVF.Arity n) =>
  Finite n -> a -> a -> SmallVec n a
basisSmallVec (k :: Finite n) zero' one' = DVF.generate $ \l ->
  if fromInteger (getFinite k) == l
    then one'
    else zero'

lensTolongVec :: forall dt t dx x n. (Additive dx, KnownNat n) =>
  LensD dt t (LongVec n dx) (LongVec n x) -> LongVec n (LensD dt t dx x)
lensTolongVec (LensD lens) = DVGS.generate $ \k ->
  LensD $ \t -> let 
      (v, dvt :: LongVec n dx -> dt) = lens t
    in (DVGS.index v k, dvt . basisLongVec k zero)

type instance Tangent (SmallVec n a) = SmallVec n (Tangent a)

type instance Tangent (LongVec n a) = LongVec n (Tangent a)

lensToSmallVec :: forall dt t dx x n. (Additive dx, DVF.Arity n) =>
  LensD dt t (SmallVec n dx) (SmallVec n x) -> SmallVec n (LensD dt t dx x)
lensToSmallVec (LensD lens) = DVF.generate $ \k ->
  LensD $ \t -> let 
      (v, dvt :: SmallVec n dx -> dt) = lens t
      finiteK = Finite (toInteger k)
    in (DVF.basicIndex v k, dvt . basisSmallVec finiteK zero)

longVecLensIso :: (KnownNat n, Additive dx, Additive dt) =>
  Iso' (LensD dt t (DVGS.Vector DV.Vector n dx) (DVGS.Vector DV.Vector n x)) (DVGS.Vector DV.Vector n (LensD dt t dx x))
longVecLensIso = iso lensTolongVec longVecToLens

smallVecLensIso :: (DVF.Arity n, Additive dx, Additive dt) =>
  Iso' (LensD dt t (SmallVec n dx) (SmallVec n x)) (SmallVec n (LensD dt t dx x))
smallVecLensIso = iso lensToSmallVec smallVecToLens


--instance Lensable (LongVec n) (LongVec n) where
instance (KnownNat n, Additive da) =>
  Lensable (DVGS.Vector DV.Vector n da) (DVGS.Vector DV.Vector n a) where
    type LensableType dt t (DVGS.Vector DV.Vector n da) (DVGS.Vector DV.Vector n a) =
      DVGS.Vector DV.Vector n (LensD dt t da a)
    lensIso = iso longVecToLens lensTolongVec
  
--instance (DVF.Arity n) =>
--  Lensable (DVFB.Vec n) (DVFB.Vec n) where
--    lensIso = iso smallVecToLens lensToSmallVec

instance (DVF.Arity n, Additive da) =>
  Lensable (DVFB.Vec n da) (DVFB.Vec n a) where
    type LensableType dt t (DVFB.Vec n da) (DVFB.Vec n a) =
      DVFB.Vec n (LensD dt t da a)
    lensIso = iso smallVecToLens lensToSmallVec

-- |
-- >>> import Prelude (Int)
-- 
-- >>> smallVecBasis' 0 :: SmallVec 1 Int
-- fromList [1]
-- 
-- >>> smallVecBasis' 1 :: SmallVec 5 Int
-- fromList [0,1,0,0,0]
smallVecBasis' :: (Distributive a, DVF.Arity n) => 
  Finite n -> SmallVec n a
smallVecBasis' (Finite i) = 
  vector $ apply (\(Const j) -> (if j == zero then one else zero, Const (j - 1))) (Const i)

smallVecBasis :: (DVF.Arity n) => 
  Finite n -> a -> a -> SmallVec n a
smallVecBasis (Finite k) zero' one' = DVF.generate $ \l -> 
  if k == fromIntegral l
    then one'
    else zero' 

longVecBasis :: (DVG.Vector v a, KnownNat n) => 
  Finite n -> a -> a -> DVGS.Vector v n a
longVecBasis k zero' one' = DVGS.generate $ \l -> 
  if k == l
    then one'
    else zero' 


instance (DVF.Arity n, Basis a) => 
  Basis (SmallVec n a) where
    type End (SmallVec n a) b = SmallVec n (End a b)
    initBackProp :: forall b. (SmallVec n a -> b) -> SmallVec n (End a b)
    initBackProp bp = DVF.generate (\k -> initBackProp (bp . smallVecBasis (Finite $ fromIntegral k) zeroBackProp))
    zeroBackProp :: SmallVec n a
    zeroBackProp = DVF.replicate zeroBackProp

instance (KnownNat n, Basis a) => 
  Basis (LongVec n a) where
    type End (LongVec n a) b = LongVec n (End a b)
    initBackProp :: forall b. (LongVec n a -> b) -> LongVec n (End a b)
    initBackProp bp = DVGS.generate (\k -> initBackProp (bp . longVecBasis k zeroBackProp))
    zeroBackProp :: LongVec n a
    zeroBackProp = DVGS.replicate zeroBackProp
--    type End (LongVec n a) b = LongVec n (End a b)
--    initBackProp :: forall b. (LongVec n a -> b) -> LongVec n (End a b)
--    initBackProp bp = undefined --     initBackProp (bp . DVGS.convert :: LongVec n a -> b)
--    zeroBackProp :: LongVec n a
--    zeroBackProp = DVGS.replicate zeroBackProp

--class Lensable dx x where
--  type LensableType dt t dx x :: Type
--  lensIso :: (Additive dt) =>
--    Iso' (LensableType dt t dx x) (LensD dt t dx x)

--instance (Additive dx0, Additive dx1) =>
--  Lensable (dx0, dx1) (x0, x1) where
--    type LensableType dt t (dx0, dx1) (x0, x1) = (LensD dt t dx0 x0, LensD dt t dx1 x1)
--    lensIso = iso tupleToLens lensToTuple

