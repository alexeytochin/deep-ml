{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module InfBackprop.Vector where

import InfBackprop.LensD (LensD(LensD), unpackLensD, Lensable, LensableType, lensIso)
import NumHask (Additive, sum, zero)
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
import Data.Vector.Fixed.Cont (ArityPeano, Peano, PeanoNum(S))
import InfBackprop.Tangent (T)
import Optics (iso)
import GHC.Natural (Natural)
--import Data.HashMap.Internal.Array (Array)


--type X = Array

type LongVec (n :: Nat) a = DVGS.Vector DV.Vector n a
type SmallVec (n :: Nat) a = DVFB.Vec n a


vecToLens :: forall dt t dx x vec (n :: Nat). (Additive dt, Applicative (vec n), Foldable (vec n)) =>
  vec n (LensD dt t dx x) -> LensD dt t (vec n dx) (vec n x)
vecToLens a = LensD $ \t ->
  let (vec, dVecT) = unzip (fmap (`unpackLensD` t) a)
  in (vec, \dVec -> sum (dVecT <*> dVec))

longVecToLens :: (Additive dt, KnownNat n) =>
  LongVec n (LensD dt t dx x) -> LensD dt t (LongVec n dx) (LongVec n x)
longVecToLens = vecToLens

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

lensToSmallVec :: forall dt t dx x n. (Additive dx, DVF.Arity n) =>
  LensD dt t (SmallVec n dx) (SmallVec n x) -> SmallVec n (LensD dt t dx x)
lensToSmallVec (LensD lens) = DVF.generate $ \k ->
  LensD $ \t -> let 
      (v, dvt :: SmallVec n dx -> dt) = lens t
      finiteK = Finite (toInteger k)
    in (DVF.basicIndex v k, dvt . basisSmallVec finiteK zero)
    

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


--class Lensable dx x where
--  type LensableType dt t dx x :: Type
--  lensIso :: (Additive dt) =>
--    Iso' (LensableType dt t dx x) (LensD dt t dx x)

--instance (Additive dx0, Additive dx1) =>
--  Lensable (dx0, dx1) (x0, x1) where
--    type LensableType dt t (dx0, dx1) (x0, x1) = (LensD dt t dx0 x0, LensD dt t dx1 x1)
--    lensIso = iso tupleToLens lensToTuple

