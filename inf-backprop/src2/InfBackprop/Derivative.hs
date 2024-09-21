{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}

module InfBackprop.Derivative where

import InfBackprop.LensD (stopDiff, DFunc, LensD(LensD), tupleToLens, lensToTuple,
  tupleLensIso, idIso, identity, unpackLensD)
import InfBackprop.Tangent (T, Tangent)
import GHC.Base (undefined, (.), Type)
import Control.Composition ((.*), (.**), (.***))
import Data.Vector.Fixed.Cont (PeanoNum(Z, S), Peano)


class Initbackprop (b :: Type) where -- | a -> ta
  type End (b :: Type) (a :: Type) :: Type
  initBackProp :: forall ta. (b, T b -> ta) -> End (T b) ta
  zeroBackProp :: b -- Proxy a -> T a
--  toLens :: End (LensD dt t dtb dtb) (LensD dt t dta dta) -> LensD 




type family Differentiable (n :: PeanoNum) t x y
type instance Differentiable 'Z t x y = x -> y
type instance Differentiable ('S n) t x y = Differentiable n (DFunc t t) (DFunc t x) (DFunc t y)
--type instance Differentiable ('S n) t x y = Differentiable n t (DFunc t x) (DFunc t y)


--derivativeOp :: 
--  (DFunc x x -> DFunc x y) -> 
--  x -> (y, T y -> T x)
--derivativeOp f = unpackLensD (f identity)
--
--derivativeOp2 :: 
--  (DFunc (DFunc x x) (DFunc x x) -> DFunc (DFunc x x) (DFunc x y)) -> 
----  DFunc x x -> (DFunc x y, DFunc x (T y) -> DFunc x (T x))
--  DFunc x x -> DFunc x (DFunc x y, DFunc x (T y) -> DFunc x (T x))
--derivativeOp2 f = unpackLensD (f identity)



derivativeOp_ ::
  -- (DFunc t x -> DFunc t y) ->
  Differentiable (Peano 1) x x y ->
  (y -> T y) ->
  -- Differentiable (Peano 0) x y (T y) ->
  x -> T x
derivativeOp_ f sb x = dyx (sb y) where
  (y, dyx) = unpackLensD (f identity) x

--  (DFunc (DFunc t t) (DFunc t x) -> DFunc (DFunc t t) (DFunc t y)) ->
--  (DFunc t y -> DFunc t (T y)) ->
--  (DFunc t x -> DFunc t (T x))

derivativeOp2_ ::
  -- (DFunc (DFunc t2 t) (DFunc t2 x) -> DFunc (DFunc t2 t) (DFunc t2 y)) ->
  Differentiable (Peano 2) x x y ->
  -- (DFunc t y -> DFunc t (T y)) ->
  Differentiable (Peano 1) x y (T y) ->
  (T x -> T (T x)) ->
  -- Differentiable (Peano 0) x (T x) (T (T x)) ->
  x ->
  T x
derivativeOp2_ = derivativeOp_ .* derivativeOp_

derivativeOp3_ ::
  -- (DFunc (DFunc (DFunc x x) (DFunc x x)) (DFunc (DFunc x x) (DFunc x x)) -> DFunc (DFunc (DFunc x x) (DFunc x x)) (DFunc (DFunc x x) (DFunc x y))) ->
  Differentiable (Peano 3) x x y ->
  -- (DFunc (DFunc x x) (DFunc x y) -> DFunc (DFunc x x) (DFunc x (T y))) ->
  Differentiable (Peano 2) x y (T y) ->
  -- (DFunc x (T x) -> DFunc x (T (T x))) ->
  Differentiable (Peano 1) x (T x) (T (T x)) -> 
  -- (T x -> T (T x)) ->
  Differentiable (Peano 0) x (T x) (T (T x)) ->
  x ->
  T x
derivativeOp3_ = derivativeOp_ .** derivativeOp_ .* derivativeOp_ --   derivativeOp . derivativeOp f

derivativeOp4_ ::
  Differentiable (Peano 4) x x y ->
  Differentiable (Peano 3) x y (T y) ->
  Differentiable (Peano 2) x (T x) (T (T x)) ->
  Differentiable (Peano 1) x (T x) (T (T x)) ->
  Differentiable (Peano 0) x (T x) (T (T x)) ->
  x ->
  T x
derivativeOp4_ = derivativeOp_ .*** derivativeOp_ .** derivativeOp_ .* derivativeOp_


--derivativeOp4 = derivativeOp .*** derivativeOp .** derivativeOp .* derivativeOp --   derivativeOp . derivativeOp f
