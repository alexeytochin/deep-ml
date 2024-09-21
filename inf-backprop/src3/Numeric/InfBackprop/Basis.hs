{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}


module Numeric.InfBackprop.Basis where

  
import Data.Kind (Type)
import Prelude (Float, const, undefined, ($), (==))
import Data.Stream (Stream)
--import Data.FiniteList (BoundedStream)
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector as DV
--import Data.Vector.Fixed.Boxed (Vec)
import qualified Data.Vector.Fixed.Boxed as DVFB
import qualified Data.Vector.Fixed as DVF
import Data.Proxy (Proxy(Proxy))
import Prelude.Tools (cross, cross3)
import GHC.TypeNats (Nat, KnownNat)
import Data.Finite.Internal (Finite(Finite), getFinite)


type LongVec (n :: Nat) a = DVGS.Vector DV.Vector n a

class IsSubtype a b where
  -- type Subtype (a :: Type) :: Type

instance IsSubtype a a
  -- type Subtype

instance (IsSubtype a b) => IsSubtype [a] b

instance (IsSubtype a0 b, IsSubtype a1 b) => IsSubtype (a0, a1) b

instance (IsSubtype a b) => IsSubtype (LongVec n a) b



type family Tangent1 (a :: Type) (b :: Type) :: Type
type instance Tangent1 Float b = b
type instance Tangent1 (a0, a1) b = (Tangent1 a0 b, Tangent1 a1 b)
type instance Tangent1 (a0, a1, a2) b = (Tangent1 a0 b, Tangent1 a1 b, Tangent1 a2 b)
type instance Tangent1 [a] b = [Tangent1 a b]
type instance Tangent1 (DVFB.Vec n a) b = DVFB.Vec n (Tangent1 a b)
type instance Tangent1 (LongVec n a) b = LongVec n (Tangent1 a b)
--type instance Tangent (Stream a) = BoundedStream (Tangent a)
--type instance Tangent (BoundedStream a) = Stream (Tangent a)

--type family GetField (a :: Type) :: Type
--type instance GetField Float = Float

type Tangent a = Tangent1 a Float

type family Dual (x :: Type) :: Type
type instance Dual Float = Float
type instance Dual (a, b) = (Dual a, Dual b)
type instance Dual (a, b, c) = (Dual a, Dual b, Dual c)
type instance Dual [a] = [Dual a]
type instance Dual (DVFB.Vec n a) = DVFB.Vec n (Dual a)
type instance Dual (LongVec n a) = LongVec n (Dual a)

type Cotangent a = Dual (Tangent a)

type CT a = Cotangent a

--type Basis a :: Type
type Basis a = Tangent1 a (CT a)

type T1 a b = Tangent1 a b

class HasBasis a where
  basis :: T1 a (CT a)
  basis1 :: proxy b -> (CT a -> CT b) -> T1 a (CT b)
  
  zeroB :: proxy a -> CT a
  zeroB1 :: HasBasis b => proxy b -> T1 a (CT b)
--  zeroB :: CT a
--  zeroB1 :: forall b. HasBasis b => T1 a (CT b)
  
instance HasBasis Float where
  basis = 1
  basis1 _ f = f 1  

  zeroB _ = 0
--  zeroB = 0

  zeroB1 :: forall b proxy. HasBasis b => proxy b -> CT b
  zeroB1 _ = zeroB (Proxy @b)
--  zeroB1 :: forall b. HasBasis b => CT b
--  zeroB1 = zeroB :: CT b