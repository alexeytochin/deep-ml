module Numeric.InfBackprop.Tangent where


import Data.Kind (Type)
import Prelude (Float, const, undefined, ($), (==), (.))
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector as DV
import qualified Data.Vector.Fixed.Boxed as DVFB
import qualified Data.Vector.Fixed as DVF
import Data.Proxy (Proxy(Proxy))
import Prelude.Tools (cross, cross3)
import GHC.TypeNats (Nat, KnownNat)
import Data.Finite.Internal (Finite(Finite), getFinite) 
import Prelude hiding (map, iterate, repeat, unzip, sum, head, tail, (!!))
import Data.Stream (Stream(Cons), (<:>), repeat, iterate, map, head, tail, fromList, (!!))
import qualified Data.Stream as Stream
-- import Data.FiniteList (BoundedStream, bJoin, unit, basisStream, basis, emptyFiniteList)
import Data.StreamExtra (BoundedStream(BoundedStream), unitBoundedStream, basisStream, boundedStreamBasis, emptyBoundedStream)
import GHC.Natural (Natural)
  
  
