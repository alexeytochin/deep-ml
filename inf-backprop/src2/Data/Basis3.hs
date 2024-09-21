{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Basis3 where

import Data.Proxy (Proxy(Proxy))
import GHC.Base (Type, const)
import Prelude (Float, undefined, fmap, fst, zip, (==), snd, ($), (.))
import NumHask.Algebra.Additive (zero)
import GHC.Natural (Natural)
import Control.Monad (void)
import NumHask (Divisive, Additive, ExpField, (/), (^), (+), sqrt, Multiplicative, (*), (-), Distributive, Subtractive, 
  (-), one)
import Data.Stream (Stream, (<:>), repeat, iterate, map)

import InfBackprop.Tangent (Tangent, T)
import Data.Vector.Generic.Sized (generate, replicate, Vector(..))
--import Data.Vector.Generic (generate, replicate) --, Vector)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector as DV
import GHC.TypeNats (KnownNat)
import Data.Finite.Internal (Finite)
--import Numeric.LinearAlgebra.Static (R(..))
import qualified Data.Vector.Fixed.Boxed as VFB
-- import Data.Vector.Fixed.Boxed (Vec(..))
import qualified Data.Vector.Fixed as VF
import Data.Primitive.SmallArray (SmallArray(..))
import Data.Vector.Fixed.Cont (Arity)
import Debug.SimpleExpr (SimpleExpr)
import Data.FiniteList (BoundedStream, emptyFiniteList, unit, bJoin)


class Basis (b :: Type) where -- | a -> ta
  type End (b :: Type) (a :: Type) :: Type
--  startBackProp :: 
  initBackProp :: forall a. (b -> a) -> End b a
  zeroBackProp :: b -- Proxy a -> T a
--  toLens ::

--class Basis a Scalar a

instance Basis Float where
  type End Float b = b
  initBackProp f = f 1
  zeroBackProp = 0

type instance Tangent (a0, a1) = (Tangent a0, Tangent a1)

instance forall a1 a2. (Basis a1, Basis a2) =>
  Basis (a1, a2) where
    type End (a1, a2) b = (End a1 b, End a2 b)
    initBackProp :: ((a1, a2) -> b) -> (End a1 b, End a2 b)
    initBackProp bp = (
        initBackProp (\a1 -> bp (a1, zeroBackProp)),
        initBackProp (\a2 -> bp (zeroBackProp, a2))
      )
    zeroBackProp :: (a1, a2)
    zeroBackProp = (zeroBackProp, zeroBackProp)

instance (Basis a0, Basis a1, Basis a2) =>
  Basis (a0, a1, a2) where
    type End (a0, a1, a2) b = (End a0 b, End a1 b, End a2 b)
    initBackProp :: ((a0, a1, a2) -> b) -> (End a0 b, End a1 b, End a2 b)
    initBackProp bp = (
        initBackProp (\a0 -> bp (a0, zeroBackProp, zeroBackProp)),
        initBackProp (\a1 -> bp (zeroBackProp, a1, zeroBackProp)),
        initBackProp (\a2 -> bp (zeroBackProp, zeroBackProp, a2))
      )
    zeroBackProp :: (a0, a1, a2)
    zeroBackProp = (zeroBackProp, zeroBackProp, zeroBackProp)

---- functions
--instance forall r a. (Basis a) =>
--  Basis (r -> a) where
--    type End (r -> a) b = r -> End a b
--    initBackProp :: ((r -> a) -> b) -> (r -> End a b)
--    initBackProp bp = \r -> initBackProp 
----     (
----        initBackProp (\a1 -> bp (a1, zeroBackProp)),
----        initBackProp (\a2 -> bp (zeroBackProp, a2))
----      )
--    zeroBackProp :: (a1, a2)
--    zeroBackProp = (zeroBackProp, zeroBackProp)


enumlist :: [a] -> [()]
enumlist = void

counter :: [a] -> [Natural]
counter l = fmap fst (zip [(0 :: Natural) ..] l)

basisList :: a -> (b -> a) -> Natural -> [b] -> [a]
basisList one_ mkZero_ i l = [if i == j then one_ else mkZero_ a | (j, a) <- zip [(0 :: Natural) ..] l]

--oneHotStream :: a -> a -> Natural -> Stream a
--oneHotStream zero' one' n = case n of
--  0 -> one' <:> repeat zero
--  _ -> zero' <:> oneHotStream zero' one' (n - 1)








--class Temp a
-- 
--instance forall a b v. (Temp a) => VG.Vector v (End a b)

type Vec n a = Vector DV.Vector n a


--temp :: VG.Vector -- Vec 2 Float
--temp = Vector ()


myBasisVector :: (VG.Vector v a, KnownNat n) => 
  Finite n -> a -> a -> Vector v n a
myBasisVector k zero' one' = generate $ \l -> 
  if k == l
    then one'
    else zero' 


--instance (VG.Vector DV.Vector a, KnownNat n, Basis a) =>
--  Basis (Vec n a) where
--    type End (Vec n a) b = Vec n (End a b)
--    initBackProp :: forall b. (Vec n a -> b) -> Vec n (End a b)
--    initBackProp bp = generate (\k -> initBackProp (bp . myBasisVector k zeroBackProp))
--    zeroBackProp :: Vec n a
--    zeroBackProp = replicate zeroBackProp


tempV3 :: a -> VFB.Vec3 a
tempV3 x = VF.convert (x, x, x)

--myBasisVec3 ::
--  Finite 2 -> a -> a -> VFB.Vec2 a
--myBasisVec3 k zero' one' = case k of
--  0 -> VFB.mk3 (one' zero')
--  1 -> VFB.mk3 (zero' one')
--  generate $ \l ->
--  if k == l
--    then one'
--    else zero'

--instance (Additive a) => Additive (VFB.Vec 2 a) where
--  (+) = VF.zipWith (+)
--  zero = VF.replicate zero

--type instance Tangent (VFB.Vec n a) = VFB.Vec n (T a)

--type family End (Vec 2 a) b = () --  VFB.Vec2 (End a b)
--
--instance forall a. (Basis a) => 
--  Basis (VFB.Vec 2 a) where
--    type End (VFB.Vec 2 a) b = VFB.Vec 2 (End a b)
--    initBackProp :: forall b. (VFB.Vec 2 a -> b) -> VFB.Vec 2 (End a b)
--    initBackProp bp = VF.convert $ initBackProp (bp . VF.convert :: VFB.Vec 2 a -> b)                
--    zeroBackProp :: VFB.Vec 2 a
--    zeroBackProp = VF.convert (zeroBackProp, zeroBackProp)

type instance Tangent SimpleExpr = SimpleExpr

instance Basis SimpleExpr where
  type End SimpleExpr b = b
  initBackProp f = f one
  zeroBackProp = zero

--instance SimpleExpr => 
--  Basis SimpleExpr where
--    type End SimpleExpr b = SimpleExpr
--    initBackProp :: forall b. (SimpleExpr -> b) -> SimpleExpr
--    initBackProp bp = VF.convert $ initBackProp (bp . VF.convert :: VFB.Vec 2 a -> b)                
--    zeroBackProp :: VFB.Vec 2 a
--    zeroBackProp = VF.convert (zeroBackProp, zeroBackProp)
















--myBasis1 :: (((Float, Float, Float), (Float, Float, Float)), ((Float, Float, Float), (Float, Float, Float)) -> b) -> ((b, b, b), (b, b, b))
--myBasis1 = basis
--
--myBasis2 :: ([(Float, (Float, Float))], [(Float, (Float, Float))] -> b) -> [(b, (b, b))]
--myBasis2 = basis


-- Datatypes
--data L2Normed a = UnsafeL2Normed {x_ :: a, y_ :: a, z_ :: a}
--getX :: (Divisive a, Additive a, ExpField a) =>
--  L2Normed a -> a
--getX (UnsafeL2Normed x_ y_ z_) = x_ / sqrt (x_^2 + y_^2 + z_^2)
--getValues :: (Divisive a, Additive a, ExpField a) =>
--  L2Normed a -> (a, a, a)
--getValues (UnsafeL2Normed x_ y_ z_) = (x_ / d, y_ / d, z_ / d) where d = sqrt (x_^2 + y_^2 + z_^2)
--
--data TangentL2Normed a = UnsafeTangentL2Normed {tx_ :: a, ty_ :: a, tz_ :: a}
--getTangentValues :: (Divisive a, Additive a, ExpField a) =>
--  L2Normed a -> TangentL2Normed a -> (a, a, a)
--getTangentValues lna tlna = undefined

--data Vector3 a = Vector3 {x_ :: a, y_ :: a, z_ :: a}
--
--type instance Tangent (Vector3 a) = Vector3 (Tangent a)
--
--instance Additive a => 
--  Additive (Vector3 a) where
--    (Vector3 ax ay az) + (Vector3 bx by bz) = Vector3 (ax + bx) (ay + by) (az + bz)
--    zero = Vector3 zero zero zero
--
--instance Subtractive a =>
--  Subtractive (Vector3 a) where
--    (Vector3 ax ay az) - (Vector3 bx by bz) = Vector3 (ax - bx) (ay - by) (az - bz)
--
--vector3Dot :: (Distributive a) => 
--  Vector3 a -> Vector3 a -> a
--vector3Dot (Vector3 ax ay az) (Vector3 bx by bz) = ax * bx + ay * by + az * bz 
--
--scalarMul :: Multiplicative a => a -> Vector3 a -> Vector3 a
--scalarMul s (Vector3 x y z) = Vector3 (s * x) (s * y) (s * z)
--
--newtype NormedVector3 a = NormedVector3 {unsafeNV :: Vector3 a} 
--
--getNormedVector3 :: (ExpField a) =>
--  NormedVector3 a -> Vector3 a
--getNormedVector3 (NormedVector3 (Vector3 x_ y_ z_)) = 
--  Vector3 (x_ / d) (y_ / d) (z_ / d) where d = sqrt (x_ * x_ + y_ * y_ + z_ * z_)
--
--data ConstraintVector3 a b = ConstraintVector3 {
--    constraint  :: NormedVector3 a,
--    vector      :: Vector3 b
--  }
--
--getConstraintVector3 :: (ExpField a) =>
--  ConstraintVector3 a a -> Vector3 a
--getConstraintVector3 (ConstraintVector3 cv x) =
--  x - scalarMul (vector3Dot x c) c where
--    c = getNormedVector3 cv

--    in x - scalarMul (vectorDot x v) v

--type instance Tangent (Vector3 a) = Vector3 (Tangent a)
--type instance Tangent (NormedVector3 a) = ConstraintVector3 a (Tangent a)

--type instance Tangent (ConstraintVector a) = ConstraintVector (Tangent a)


--mkConstraintVector :: Vector a -> ConstraintVector a
--mkConstraintVector x =
--  UnsafeConstraintVector $ \nv -> let
--      v = getNormedVector nv

--instance Basis a => 
--  Basis (Vector3 a) where
--    type End (Vector3 a) b = Vector3 (End a b)
    
    -- basis   :: forall b. (a, T a -> b) -> End a b
--    basis :: (Vector a, Vector (T a) -> b) -> Vector (End a b)
--    basis (Vector x y z, bp) = Vector dx dy dz where
--      dx = basis (x, \dx' -> bp (Vector dx' (mkZero y) (mkZero z)))
--      dy = basis (y, \dy' -> bp (Vector (mkZero x) dy' (mkZero z)))
--      dz = basis (z, \dz' -> bp (Vector (mkZero x) (mkZero y) dz'))
--    mkZero :: Vector a -> Vector (T a)
--    mkZero (Vector x y z) = Vector (mkZero x) (mkZero y) (mkZero z)


--instance (ExpField a, Basis a) =>
--  Basis (NormedVector3 a) where
--    type End (NormedVector3 a) b = ConstraintVector3 a (End a b)

    -- basis   :: forall b. (a, T a -> b) -> End a b
    -- basis :: (NormedVector a, ConstraintVector (T a) -> b) -> End (a b)
--        basis ((x1, x2), f) = (
--            basis (x1, \dx1 -> f (dx1, mkZero x2)),
--            basis (x2, \dx2 -> f (mkZero x1, dx2))
--          )
--    basis :: (NormedVector a, ConstraintVector a (T a) -> b) -> ConstraintVector a (End a b)
--    basis (v, bp) = ConstraintVector v (Vector dx dy dz) where
--        dx = basis (x, \dx' -> bp (ConstraintVector v (Vector dx' (mkZero y) (mkZero z))))
--        dy = basis (y, \dy' -> bp (ConstraintVector v (Vector (mkZero x) dy' (mkZero z))))
--        dz = basis (z, \dz' -> bp (ConstraintVector v (Vector (mkZero x) (mkZero y) dz')))
--        Vector x y z = getNormedVector v
--    mkZero :: NormedVector a -> ConstraintVector a (T a)
--    mkZero nv = ConstraintVector nv (Vector (mkZero x) (mkZero y) (mkZero z)) where
--      (Vector x y z) = getNormedVector nv

--instance Basis (TangentL2Normed a) where
--    type End (TangentL2Normed a) b = TangentL2Normed (End a b)

