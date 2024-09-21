module NumHask.Extra where 
--  (
--  IntegralPower,
--  integralPow,
--  intPow,
--) where

import GHC.Base (Ord, Float, Int, Double)
import NumHask (Integral, (^^), Subtractive, Divisive)
import Debug.SimpleExpr (SimpleExpr)


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
