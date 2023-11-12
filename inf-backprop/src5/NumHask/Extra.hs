{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC 
    -fno-warn-unused-imports 
    -fno-warn-missing-export-lists
    -fno-warn-orphans
  #-}

module NumHask.Extra where 
--  (
--  IntegralPower,
--  integralPow,
--  intPow,
--) where

import GHC.Base (Ord, Float, Int, Double, (==), ($), undefined, Maybe(Just, Nothing))
import NumHask (Additive, (+), zero, Multiplicative, (*), one, Integral, (^^), Subtractive, Divisive, Multiplicative)
import Debug.SimpleExpr (SimpleExpr)
import GHC.TypeNats (KnownNat)
import Data.Finite (Finite)
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic as DVG
import Numeric.InfBackprop.Tangent (LongVec, BoxedVec)
import qualified Data.Stream as DS


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

instance (Ord p, Subtractive p, Integral p) =>
  IntegralPower p SimpleExpr where
    integralPow n x = x ^^ n
  
intPow :: IntegralPower Int a => Int -> a -> a
intPow = integralPow

--longVecBasis :: (DVG.Vector v a, KnownNat n) =>
--  Finite n -> a -> a -> DVGS.Vector v n a
--longVecBasis k zero' one' = DVGS.generate $ \l ->
--  if k == l
--    then one'
--    else zero'
vecBasis :: (DVG.Vector v a, KnownNat n) =>
  Finite n -> a -> a -> DVGS.Vector v n a
vecBasis k zero' one' = DVGS.generate $ \l ->
  if k == l
    then one'
    else zero'

longVecSum :: Additive a => BoxedVec n a -> a
longVecSum = DVGS.foldl' (+) zero


-- Orphane instancies
instance (Additive a0, Additive a1) => 
  Additive (a0, a1) where
    zero = (zero, zero)
    (x0, x1) + (y0, y1) = (x0 + y0, x1 + y1)

instance (Additive a0, Additive a1, Additive a2) => 
  Additive (a0, a1, a2) where
    zero = (zero, zero, zero)
    (x0, x1, x2) + (y0, y1, y2) = (x0 + y0, x1 + y1, x2 + y2)

instance (KnownNat n, Additive a) => 
  Additive (BoxedVec n a) where
    zero = DVGS.replicate zero
    (+) = DVGS.zipWith (+)

instance (KnownNat n, Multiplicative a) => 
  Multiplicative (BoxedVec n a) where
    one = DVGS.replicate one
    (*) = DVGS.zipWith (*)

#if MIN_VERSION_numhask(0,11,0)
#else
instance (Distributive a) =>
  Distributive (BoxedVec n a)
#endif


instance (Additive a) => 
  Additive (DS.Stream a) where
    zero = DS.repeat zero
    (+) = DS.zipWith (+)

instance (Additive a) => 
  Additive (Maybe a) where
    zero = Just zero
    (Just a) + (Just b) = Just (a + b)
    _ + _ = Nothing


