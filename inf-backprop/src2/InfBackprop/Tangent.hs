module InfBackprop.Tangent where

import Data.Kind (Type)
import Prelude (Float)
import Data.Stream (Stream)
import Data.FiniteList (BoundedStream)


type family Tangent (x :: Type) :: Type
type instance Tangent Float = Float
type instance Tangent (a, b) = (Tangent a, Tangent b)
type instance Tangent (a, b, c) = (Tangent a, Tangent b, Tangent c)
type instance Tangent [a] = [Tangent a]
type instance Tangent (Stream a) = BoundedStream (Tangent a)
type instance Tangent (BoundedStream a) = Stream (Tangent a)



--type family B (f :: k) (b :: Type) :: Type
--type instance B (,) b = (b, b)
--
--type family F (a :: Type) :: k
--type instance F (a1, a2) = (,)

--type family End (a :: Type) (b :: Type) :: Type
--type instance End Float b = b
--type instance End (a1, a2) b = (End a1 b, End a2 b)
--type instance End (a0, a1, a2) b = (End a0 b, End a1 b, End a2 b)
--type instance End [a] b = [End a b]

type T a = Tangent a