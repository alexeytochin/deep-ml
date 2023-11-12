--{-# LANGUAGE UndecidableInstances #-}


module Numeric.InfBackprop.DLens where

import Data.Profunctor (Profunctor, dimap, Strong, first', second', Costrong, unfirst, unsecond)
import NumHask ((*), (+), sin, cos, Additive, zero, Multiplicative, one, ExpField, exp, log, (**), sqrt, sin, cos, sinh,
    asin, acos, cosh, atan, atan2, asinh, acosh, atanh, pi,
    Subtractive, negate, (-),
    Divisive, recip, (/), Field, AdditiveAction, (^), (^^), TrigField, two, Distributive, Integral, fromIntegral, FromIntegral
  )
import Prelude (fst, (.), ($), snd, const, curry, id, uncurry, Int, Ord, Float, Double, Bool, Integer, Word, undefined, Functor, fmap, flip)
import qualified Prelude as P
import Data.Bifunctor (bimap)
import Prelude.Tools (fork, cross, assoc, disassoc)
import Optics (Iso', iso)
import Data.Functor.Classes (Show1)
import Data.Kind (Type)
import Data.Tuple.Extra (curry3)
import GHC.Natural (Natural)
import GHC.Int (Int8, Int16, Int32, Int64)
import GHC.Word (Word8, Word16, Word32, Word64)
import Debug.SimpleExpr (SimpleExpr, number)
import Data.Data (Proxy(Proxy))
import GHC.TypeNats (Nat)
import Control.PolyFunctor (binarryFfmap1, BinnaryVectorizable1)
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector as DV
import qualified Data.Vector.Fixed.Boxed as DVFB
import qualified Data.Vector.Fixed as DVF
import Numeric.InfBackprop.Tangent (Tangent, Dual)

type Vec (n :: Nat) a =
 DVGS.Vector DV.Vector n a


--type family GetField (a :: Type) :: Type
--type instance GetField Float = Float


--type family Tangent1 (a :: Type) (b :: Type) :: Type
--type instance Tangent1 Float b = b
--type instance Tangent1 Float b = b
--type instance Tangent1 (a0, a1) b = (Tangent1 a0 b, Tangent1 a1 b)
--type instance Tangent1 (a0, a1, a2) b = (Tangent1 a0 b, Tangent1 a1 b, Tangent1 a2 b)
--type instance Tangent1 [a] b = [Tangent1 a b]
--type instance Tangent1 (DVFB.Vec n a) b = DVFB.Vec n (Tangent1 a b)
--type instance Tangent1 (LongVec n a) b = LongVec n (Tangent1 a b)
----type instance Tangent1 (Stream a) b = Stream (Tangent1 a b)
----type instance Tangent1 (BoundedStream a) b = BoundedStream (Tangent1 a b)




--type Tangent a = Tangent1 a Float
--
--type Cotangent a = Dual (Tangent a)
--
--type CT a = Cotangent a
--
----type Basis a :: Type
--type Basis a = Tangent1 a (CT a)
--
--type T1 a b = Tangent1 a b


data DLens cx x cy y = forall cache. MkDLens {
    forward :: x -> (y, cache),
    backward :: (cy, cache) -> cx 
  }
  
--type instance Tangent1 (DLens ct t ca a) b = DLens ct t (Tangent1 ca b) (Tangent1 a b)
type instance Tangent (DLens ct t ca a) = DLens ct t (Tangent ca) (Tangent a)
type instance Dual (DLens ct t ca a) = DLens ct t (Dual ca) (Dual a)

  
call :: DLens cx x cy y -> x -> y
call (MkDLens f _) = fst . f

lensToDerivative :: DLens ca a cb b -> (b -> cb) -> a -> ca
lensToDerivative (MkDLens f b) cvf = b . cross cvf id . f

lensToDerivativeTuple :: DLens ca a cb b -> a -> (b, cb -> ca)
lensToDerivativeTuple (MkDLens f b) x = (y, bp) where
  bp cy = b (cy, h) 
  (y, h) = f x 

idDLens :: DLens cx x cx x
idDLens = MkDLens (, ()) fst

(%) :: forall x cx y cy z cz. DLens cy y cz z -> DLens cx x cy y -> DLens cx x cz z
(MkDLens (f1 :: y -> (z, h1)) (b1 :: (cz, h1) -> cy)) % (MkDLens (f2 :: x -> (y, h2)) (b2 :: (cy, h2) -> cx)) = MkDLens f3 b3 where
  f3 :: x -> (z, (h1, h2))
  f3 = assoc . cross f1 id . f2
  b3 :: (cz, (h1, h2)) -> cx
  b3 = b2 . cross b1 id . disassoc



derivativeOp ::
  (DLens ca a ca a -> DLens ca a cb b) ->
  (b -> cb) ->
  a ->
  ca
derivativeOp f sb x = dyx (sb y) where
  (y, dyx) = derivativeTuple f x

derivativeTuple :: forall a ca b cb.
  (DLens ca a ca a -> DLens ca a cb b) ->
  a ->
  (b, cb -> ca)
derivativeTuple func = lensToDerivativeTuple (func idDLens)


lensToUnnaryFunc :: DLens dx x dy y -> DLens dt t dx x -> DLens dt t dy y
lensToUnnaryFunc = (%)




scalarDFunc :: Multiplicative a =>
  (a -> a) -> (a -> a) -> DLens a a a a
scalarDFunc f f' = MkDLens (fork f f') (uncurry (*))

constDFunc :: Additive ct => 
  a -> DLens ct t ca a
constDFunc c = MkDLens (const (c, ())) (const zero)

--instance (Additive x, Additive (CT t), BinnaryVectorizable1 (CT t) ct) =>
--  Additive (DLens ct t cx x) where
--    (MkDLens (f1 :: t -> (x, h1)) (b1 :: (cx, h1) -> ct)) + (MkDLens (f2 :: t -> (x, h2)) (b2 :: (cx, h2) -> ct)) = MkDLens f b where
--      f t = (y1 + y2, (h1, h2)) where 
--        (y1, h1) = f1 t 
--        (y2, h2) = f2 t
--      b (cy, (h1, h2)) = binarryFfmap1 ((+) :: CT t -> CT t -> CT t) (b1 (cy, h1)) (b2 (cy, h2))
--    zero = constDFunc zero
    
--instance (Additive x, Additive (CT t)) =>
--  Additive (DLens (CT t) t (CT x) x) where
--    (MkDLens (f1 :: t -> (x, h1)) (b1 :: (cx, h1) -> ct)) + (MkDLens (f2 :: t -> (x, h2)) (b2 :: (cx, h2) -> ct)) = MkDLens f b where
--      f t = (y1 + y2, (h1, h2)) where 
--        (y1, h1) = f1 t 
--        (y2, h2) = f2 t
--      b (cy, (h1, h2)) = binarryFfmap1 ((+) :: CT t -> CT t -> CT t) (b1 (cy, h1)) (b2 (cy, h2))
--    zero = constDFunc zero

--instance (Subtractive x, Additive ct, Subtractive cx) =>
--  Subtractive (DLens ct t cx x) where
--    (MkDLens (f1 :: t -> (x, h1)) (b1 :: (cx, h1) -> ct)) - (MkDLens (f2 :: t -> (x, h2)) (b2 :: (cx, h2) -> ct)) = MkDLens f b where
--      f t = (y1 - y2, (h1, h2)) where 
--        (y1, h1) = f1 t 
--        (y2, h2) = f2 t
--      b (cy, (h1, h2)) = b1 (cy, h1) + b2 (negate cy, h2)
--    negate (MkDLens f b) = MkDLens (cross negate id . f) (b . cross negate id)
--
--instance (
--    Multiplicative a, 
--    Additive ct
--  ) => 
--    Multiplicative (DLens ct t a a) where
--      (MkDLens (f1 :: t -> (a, h1)) (b1 :: (a, h1) -> ct)) * (MkDLens (f2 :: t -> (a, h2)) (b2 :: (a, h2) -> ct)) = MkDLens f b where
--        f t = (y1 * y2, (h1, y1, h2, y2)) where
--          (y1, h1) = f1 t
--          (y2, h2) = f2 t
--        b (cy, (h1, y1, h2, y2)) = b1 (cy * y2, h1) + b2 (y1 * cy, h2)
--      one = constDFunc one
--
--instance (
--    Divisive a, 
--    Subtractive a, 
--    Additive ct --,
--    -- Multiplicative (DLens ct t a a)
--  ) =>
--    Divisive (DLens ct t a a) where
--      (MkDLens (f1 :: t -> (x, h1)) (b1 :: (cx, h1) -> ct)) / (MkDLens (f2 :: t -> (x, h2)) (b2 :: (cx, h2) -> ct)) = MkDLens f b where
--        f t = (y1 / y2, (h1, y1, h2, y2)) where 
--          (y1, h1) = f1 t 
--          (y2, h2) = f2 t
--        b (cy, (h1, y1, h2, y2)) = b1 (cy / y2, h1) + b2 (negate y1 * cy / (y2 * y2), h2)
--
--instance (
--    Additive ct, 
--    Subtractive a,
--    TrigField a
--  ) =>
--    TrigField (DLens ct t a a) where
--      sin = lensToUnnaryFunc $ scalarDFunc sin cos
--      cos = lensToUnnaryFunc $ scalarDFunc cos (negate . sin)
--      asin = undefined
--      acos = undefined
--      atan = undefined
--      atan2 = undefined
--      sinh = lensToUnnaryFunc $ scalarDFunc cosh sinh
--      cosh = lensToUnnaryFunc $ scalarDFunc sinh cosh
--      asinh = undefined
--      acosh = undefined
--      atanh = undefined
--      pi = constDFunc pi