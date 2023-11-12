module Numeric.InfBackprop.Linear where

import Prelude (Float, undefined, id)
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector as DV
import qualified Data.Vector.Fixed.Boxed as DVFB
import qualified Data.Vector.Fixed as DVF
import GHC.Base (Type)


type family Dual (x :: Type) :: Type
type instance Dual Float = Float
type instance Dual (a, b) = (Dual a, Dual b)
type instance Dual (a, b, c) = (Dual a, Dual b, Dual c)
type instance Dual [a] = [Dual a]
type instance Dual (DVFB.Vec n a) = DVFB.Vec n (Dual a)


type family Basis f a b :: Type
type instance Basis Float Float b = b
type instance Basis f (a0, a1) b = (Basis f a0 b, Basis f a1 b)
type instance Basis f (a0, a1, a2) b = (Basis f a0 b, Basis f a1 b, Basis f a2 b)
type instance Basis f [a] b = [Basis f a b]
type instance Basis f (DVFB.Vec n a) b = DVFB.Vec n (Basis f a b)



data Linear f a b = UnsafeMkLinear {
    call :: a -> b,
    basis :: Basis f a b
  } 
  
--identity :: Linear f a a
--identity = UNsafeMkLinear id undefined
--  
--  
--  
--example1 :: Linear Float (DVFB.Vec n Float) (DVFB.Vec m Float)
--example1 = UNsafeMkLinear undefined undefined