{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.InfBackprop.DFunc where

import Data.Kind (Type)
import Prelude (Float, undefined, ($), const, fromIntegral, (==))
import GHC.TypeLits (KnownNat, Nat, type (+))
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Fixed.Cont as DVFC
import qualified Data.Vector.Fixed.Boxed as DVFB
import qualified Data.Vector.Fixed as DVF
import qualified Data.Vector as DV
import NumHask (sin, cos, Additive, zero, TrigField, Distributive, (+), (*))
import Data.Functor.Identity (Identity)
import Data.Finite.Internal (Finite(Finite), getFinite)


type LongVec (n :: Nat) a = DVGS.Vector DV.Vector n a
type SmallVec (n :: Nat) a = DVFB.Vec n a


--type family ContangentVectorField a :: Type

type ContangentVectorField a = ContangentVectorField2 a a
type CVF a = ContangentVectorField a


type family ContangentVectorField2 (a :: Type) (b :: Type) :: Type
type instance ContangentVectorField2 a Float = a -> a
type instance ContangentVectorField2 a (DVFB.Vec n b) = ContangentVectorField2 a b

--type instance ContangentVectorField Float = Float -> Float
--type instance ContangentVectorField (DVFB.Vec n a) =
--  DVFB.Vec n (ContangentVectorField a) -> DVFB.Vec n (ContangentVectorField a)
--type instance ContangentVectorField (a -> b) = a -> ContangentVectorField b


--type instance Vectorize (DVFB.Vec n) Float = DVFB.Vec n Float
--type instance Vectorize (DVFB.Vec n) (a -> b) = a -> Vectorize (DVFB.Vec n) b

class Vectorization (v :: Type -> Type) where
  type Vectorize (v :: Type -> Type) a :: Type
  vectoirize :: (v a -> v b) -> Vectorize v a -> Vectorize v b






data DFunc a b = MkDFunc {
  call :: a -> b,
  pullback :: CVF b -> CVF a
}





class HasBasis a b where
  type FuncBasis a b :: Type
  initBackprop :: (a -> b) -> FuncBasis a b

instance HasBasis (Float -> Float) b where
  type FuncBasis (Float -> Float) b = b
  initBackprop pb = pb (const 1)

smallVecBasis :: (DVF.Arity n) => 
  Finite n -> a -> a -> SmallVec n a
smallVecBasis (Finite k) zero' one' = DVF.generate $ \l -> 
  if k == fromIntegral l
    then one'
    else zero' 

instance (DVF.Arity n) => 
  HasBasis (DVFB.Vec n Float -> DVFB.Vec n Float) (b -> c) where
    type FuncBasis (DVFB.Vec n Float -> DVFB.Vec n Float) (b -> c) = b -> DVFB.Vec n c
    initBackprop pb x = DVF.generate (\k -> backward (const (smallVecBasis (Finite $ fromIntegral k) 0 1))) where
      backward vf = pb vf x

-- Examples

conv :: (Distributive a, DVF.Arity n) => DVFB.Vec n a -> DVFB.Vec n a -> a
conv x_ y_ = DVFC.foldl (+) zero $ DVF.zipWith (*) (DVFC.cvec x_) (DVFC.cvec y_)

--sinDFunc1 :: DFunc Float Float
--sinDFunc1 = MkDFunc f b where
--  f :: Float -> DVFB.Vec 3 Float
--  f t = DVF.mk3 (sin t) (cos t) zero
--  b :: (DVFB.Vec 3 Float -> DVFB.Vec 3 Float) -> Float -> Float
--  b fv t = conv (fv (f t)) (DVF.mk3 (cos t) (-sin t) zero)


testDFunc1 :: DFunc Float (DVFB.Vec 3 Float)
testDFunc1 = MkDFunc f b where
  f :: Float -> DVFB.Vec 3 Float
  f t = DVF.mk3 (sin t) (cos t) zero
  b :: (DVFB.Vec 3 Float -> DVFB.Vec 3 Float) -> Float -> Float
  b fv t = conv (fv (f t)) (DVF.mk3 (cos t) (-sin t) zero)