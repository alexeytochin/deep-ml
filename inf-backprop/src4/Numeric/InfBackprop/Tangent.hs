module  Numeric.InfBackprop.Tangent where

import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector as DV
import qualified Data.Vector.Fixed.Boxed as DVFB
import GHC.TypeNats (Nat)
import GHC.Base (Type, Float)
import Data.Stream (Stream)
import Data.StreamExtra (BoundedStream(BoundedStream), unitBoundedStream, basisStream, boundedStreamBasis, emptyBoundedStream)
import Debug.SimpleExpr (SimpleExprF, SimpleExpr)

  
type LongVec (n :: Nat) a = DVGS.Vector DV.Vector n a

--type family Tangent1 (a :: Type) (b :: Type) :: Type
--type instance Tangent1 Float b = b
--type instance Tangent1 (a0, a1) b = (Tangent1 a0 b, Tangent1 a1 b)
--type instance Tangent1 (a0, a1, a2) b = (Tangent1 a0 b, Tangent1 a1 b, Tangent1 a2 b)
--type instance Tangent1 [a] b = [Tangent1 a b]
--type instance Tangent1 (DVFB.Vec n a) b = DVFB.Vec n (Tangent1 a b)
--type instance Tangent1 (DVGS.Vector v n a) b = DVGS.Vector v n (Tangent1 a b)
--type instance Tangent1 (Stream a) b = Stream (Tangent1 a b)
--type instance Tangent1 (BoundedStream a) b = BoundedStream (Tangent1 a b)
--type instance Tangent1 SimpleExpr b = b
--type instance Tangent1 (SimpleExprF a) b = SimpleExprF (Tangent1 a b)


--type family GetField (a :: Type) :: Type
--type instance GetField Float = Float

--type Tangent a = Tangent1 a Float

type family Tangent (a :: Type) :: Type
type instance Tangent Float = Float
type instance Tangent SimpleExpr = SimpleExpr
type instance Tangent (a0, a1) = (Tangent a0, Tangent a1)
type instance Tangent (a0, a1, a2) = (Tangent a0, Tangent a1, Tangent a2)
type instance Tangent [a] = [Tangent a]
type instance Tangent (DVFB.Vec n a) = DVFB.Vec n (Tangent a)
type instance Tangent (DVGS.Vector v n a) = DVGS.Vector v n (Tangent a)
type instance Tangent (Stream a) = Stream (Tangent a)
type instance Tangent (BoundedStream a) = BoundedStream (Tangent a)


-- type Tangent a = Tangent1 a a

type family Dual (x :: Type) :: Type
type instance Dual Float = Float
type instance Dual (a, b) = (Dual a, Dual b)
type instance Dual (a, b, c) = (Dual a, Dual b, Dual c)
type instance Dual [a] = [Dual a]
type instance Dual (DVFB.Vec n a) = DVFB.Vec n (Dual a)
type instance Dual (DVGS.Vector v n a) = DVGS.Vector v n (Dual a)
type instance Dual (Stream a) = BoundedStream (Dual a)
type instance Dual (BoundedStream a) = Stream (Dual a)
type instance Dual SimpleExpr = SimpleExpr
type instance Dual (SimpleExprF a) = SimpleExprF (Dual a)


type Cotangent a = Dual (Tangent a)

type CT a = Cotangent a

--type Basis a :: Type
--type Basis a = Tangent1 a (CT a)
--
--type T1 a b = Tangent1 a b