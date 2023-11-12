{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

module Data.Basis2 where

import Data.Proxy (Proxy(Proxy))
import GHC.Base (Type, const)
import Prelude (Float, undefined, fmap, fst, zip, (==), snd, ($))
import NumHask.Algebra.Additive (zero)
import GHC.Natural (Natural)
import Control.Monad (void)
import NumHask (Divisive, Additive, ExpField, (/), (^), (+), sqrt, Multiplicative, (*), (-), Distributive, Subtractive, 
  (-))
--import Data.Vector.Generic.Sized (Vector)
import InfBackprop.Tangent (Tangent, T)



class Basis (a :: Type) where -- | a -> ta
  type End (a :: Type) (b :: Type) :: Type
  basis   :: forall b. (a, T a -> b) -> End a b
  mkZero  :: a -> T a

instance Basis Float where
  type End Float b = b
  basis (_, f) = f 1
  mkZero = const 0

instance (Basis a1, Basis a2) =>
  Basis (a1, a2) where
    type End (a1, a2) b = (End a1 b, End a2 b)
    basis :: ((a1, a2), (T a1, T a2) -> b) -> (End a1 b, End a2 b)
    basis ((x1, x2), f) = (
        basis (x1, \dx1 -> f (dx1, mkZero x2)),
        basis (x2, \dx2 -> f (mkZero x1, dx2))
      )
    mkZero :: (a1, a2) -> (T a1, T a2)
    mkZero (x0, x1) = (mkZero x0, mkZero x1)

instance (Basis a0, Basis a1, Basis a2) =>
  Basis (a0, a1, a2) where
    type End (a0, a1, a2) b = (End a0 b, End a1 b, End a2 b)
    basis :: ((a0, a1, a2), (T a0, T a1, T a2) -> b) -> (End a0 b, End a1 b, End a2 b)
    basis ((x0, x1, x2), f) = (
        basis (x0, \dx0 -> f (dx0, mkZero x1, mkZero x2)),
        basis (x1, \dx1 -> f (mkZero x0, dx1, mkZero x2)),
        basis (x2, \dx2 -> f (mkZero x0, mkZero x1, dx2))
      )
    mkZero :: (a0, a1, a2) -> (T a0, T a1, T a2)
    mkZero (x0, x1, x2) = (mkZero x0, mkZero x1, mkZero x2)

enumlist :: [a] -> [()]
enumlist = void

counter :: [a] -> [Natural]
counter l = fmap fst (zip [(0 :: Natural) ..] l)

basisList :: a -> (b -> a) -> Natural -> [b] -> [a]
basisList one_ mkZero_ i l = [if i == j then one_ else mkZero_ a | (j, a) <- zip [(0 :: Natural) ..] l]

enumerateList :: [a] -> [(Natural, a)]
enumerateList = zip [(0 :: Natural) ..]

--listDual :: (Multiplicative a, Additive a) =>
--  ([()], [a] -> b) -> [b]
--listDual (l, f) = [unitDual (\x -> f (basisList x i l)) | i <- counter l]

instance Basis a =>
  Basis [a] where
    type End [a] b = [End a b]
    basis :: ([a], [T a] -> b) -> [End a b]
    basis (l, f) = [basis (a, \x -> f (basisList x mkZero i l)) | (i, a) <- zip [(0 :: Natural) ..] l]
    mkZero :: [a] -> [T a]
    mkZero = fmap mkZero







myBasis1 :: (((Float, Float, Float), (Float, Float, Float)), ((Float, Float, Float), (Float, Float, Float)) -> b) -> ((b, b, b), (b, b, b))
myBasis1 = basis

myBasis2 :: ([(Float, (Float, Float))], [(Float, (Float, Float))] -> b) -> [(b, (b, b))]
myBasis2 = basis


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

data Vector a = Vector {x_ :: a, y_ :: a, z_ :: a}

type instance Tangent (Vector a) = Vector (Tangent a)

instance Additive a => 
  Additive (Vector a) where
    (Vector ax ay az) + (Vector bx by bz) = Vector (ax + bx) (ay + by) (az + bz)
    zero = Vector zero zero zero

instance Subtractive a =>
  Subtractive (Vector a) where
    (Vector ax ay az) - (Vector bx by bz) = Vector (ax - bx) (ay - by) (az - bz)


vectorDot :: (Distributive a) => 
  Vector a -> Vector a -> a
vectorDot (Vector ax ay az) (Vector bx by bz) = ax * bx + ay * by + az * bz 

scalarMul :: Multiplicative a => a -> Vector a -> Vector a
scalarMul s (Vector x y z) = Vector (s * x) (s * y) (s * z)

newtype NormedVector a = NormedVector {unsafeNV :: Vector a} 

getNormedVector :: (ExpField a) =>
  NormedVector a -> Vector a
getNormedVector (NormedVector (Vector x_ y_ z_)) = 
  Vector (x_ / d) (y_ / d) (z_ / d) where d = sqrt (x_ * x_ + y_ * y_ + z_ * z_)

data ConstraintVector a b = ConstraintVector {
    constraint  :: NormedVector a,
    vector      :: Vector b
  }

getConstraintVector :: (ExpField a) =>
  ConstraintVector a a -> Vector a
getConstraintVector (ConstraintVector cv x) =
  x - scalarMul (vectorDot x c) c where
    c = getNormedVector cv

--    in x - scalarMul (vectorDot x v) v

type instance Tangent (Vector a) = Vector (Tangent a)
type instance Tangent (NormedVector a) = ConstraintVector a (Tangent a)
--type instance Tangent (ConstraintVector a) = ConstraintVector (Tangent a)


--mkConstraintVector :: Vector a -> ConstraintVector a
--mkConstraintVector x =
--  UnsafeConstraintVector $ \nv -> let
--      v = getNormedVector nv
instance Basis a => 
  Basis (Vector a) where
    type End (Vector a) b = Vector (End a b)
    -- basis   :: forall b. (a, T a -> b) -> End a b
    basis :: (Vector a, Vector (T a) -> b) -> Vector (End a b)
    basis (Vector x y z, bp) = Vector dx dy dz where
      dx = basis (x, \dx' -> bp (Vector dx' (mkZero y) (mkZero z)))
      dy = basis (y, \dy' -> bp (Vector (mkZero x) dy' (mkZero z)))
      dz = basis (z, \dz' -> bp (Vector (mkZero x) (mkZero y) dz'))
    mkZero :: Vector a -> Vector (T a)
    mkZero (Vector x y z) = Vector (mkZero x) (mkZero y) (mkZero z)


instance (ExpField a, Basis a) =>
  Basis (NormedVector a) where
    -- type End (TangentL2Normed a) b = TangentL2Normed (End a b)
    type End (NormedVector a) b = ConstraintVector a (End a b)
    -- basis   :: forall b. (a, T a -> b) -> End a b
    -- basis :: (NormedVector a, ConstraintVector (T a) -> b) -> End (a b)
--        basis ((x1, x2), f) = (
--            basis (x1, \dx1 -> f (dx1, mkZero x2)),
--            basis (x2, \dx2 -> f (mkZero x1, dx2))
--          )
    basis :: (NormedVector a, ConstraintVector a (T a) -> b) -> ConstraintVector a (End a b)
    basis (v, bp) = ConstraintVector v (Vector dx dy dz) where
        dx = basis (x, \dx' -> bp (ConstraintVector v (Vector dx' (mkZero y) (mkZero z))))
        dy = basis (y, \dy' -> bp (ConstraintVector v (Vector (mkZero x) dy' (mkZero z))))
        dz = basis (z, \dz' -> bp (ConstraintVector v (Vector (mkZero x) (mkZero y) dz')))
        Vector x y z = getNormedVector v
    mkZero :: NormedVector a -> ConstraintVector a (T a)
    mkZero nv = ConstraintVector nv (Vector (mkZero x) (mkZero y) (mkZero z)) where
      (Vector x y z) = getNormedVector nv

--instance Basis (TangentL2Normed a) where
--    type End (TangentL2Normed a) b = TangentL2Normed (End a b)

