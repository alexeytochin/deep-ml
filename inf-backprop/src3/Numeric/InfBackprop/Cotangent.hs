{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}

module Numeric.InfBackprop.Cotangent where

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


type LongVec (n :: Nat) a = DVGS.Vector DV.Vector n a


type family Dual (x :: Type) :: Type
type instance Dual Float = Float
type instance Dual (a, b) = (Dual a, Dual b)
type instance Dual (a, b, c) = (Dual a, Dual b, Dual c)
type instance Dual [a] = [Dual a]
type instance Dual (DVFB.Vec n a) = DVFB.Vec n (Dual a)
type instance Dual (LongVec n a) = LongVec n (Dual a)
type instance Dual (Stream a) = BoundedStream (Dual a)
type instance Dual (BoundedStream a) = Stream (Dual a)

--type family GetField (a :: Type) :: Type
--type instance GetField Float = Float


type family Tangent1 (a :: Type) (b :: Type) :: Type
type instance Tangent1 Float b = b
type instance Tangent1 Float b = b
type instance Tangent1 (a0, a1) b = (Tangent1 a0 b, Tangent1 a1 b)
type instance Tangent1 (a0, a1, a2) b = (Tangent1 a0 b, Tangent1 a1 b, Tangent1 a2 b)
type instance Tangent1 [a] b = [Tangent1 a b]
type instance Tangent1 (DVFB.Vec n a) b = DVFB.Vec n (Tangent1 a b)
type instance Tangent1 (LongVec n a) b = LongVec n (Tangent1 a b)
type instance Tangent1 (Stream a) b = Stream (Tangent1 a b)
type instance Tangent1 (BoundedStream a) b = BoundedStream (Tangent1 a b)


type Tangent a = Tangent1 a Float

type Cotangent a = Dual (Tangent a)

--type family Tangent2 (a :: Type) (b :: Type) :: Type
--type instance Tangent2 Float b = b
--type instance Tangent2 (a0, a1) b = (Tangent2 a0 b, Tangent2 a1 b)
--type instance Tangent2 [a] b = [Tangent2 a b]
--type instance Tangent2 (DVFB.Vec n a) b = DVFB.Vec n (Tangent2 a b)


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


type CT a = Cotangent a

--type Basis a :: Type
type Basis a = Tangent1 a (CT a)

type T1 a b = Tangent1 a b

class HasBasis a where
--  type Cotangent a :: Type
--  basis :: proxy a -> T1 a (CT a) -- Tangent1 a (Cotangent a)

  basis1 :: forall b proxy. proxy a -> proxy b -> (CT a -> CT b) -> T1 a (CT b)
--  basis1 :: forall b proxy. proxy b -> (CT a -> CT b) -> a -> T1 a (CT b)
  
  -- oneB1 :: Tangent1 a (Cotangent a)
--  oneB :: proxy a -> CT a
  zeroB :: proxy a -> CT a
--  zeroB1 :: proxy a -> proxy b -> T1 a (CT b)
--  zeroB1 :: proxy a -> CT a
--  basis :: a -> Basis a a
--  basis = basis1 (Proxy :: Proxy a)

basis :: forall a proxy. HasBasis a => proxy a -> T1 a (CT a)
basis _ = basis1 (Proxy @a) (Proxy @a) id
--basis :: forall a. HasBasis a => a -> T1 a (CT a)
--basis = basis1 (Proxy @a) id

instance HasBasis Float where
  basis1 _ _ f = f 1  
  zeroB _ = 0
--  basis1 _ f _ = f 1  
--  zeroB _ = 0


--instance (HasBasis a0, HasBasis a1) => HasBasis (a0, a1) where
--  type Cotangent (a0, a1) = (Cotangent a0, Cotangent a1)
--  type Basis (a0, a1) = (Basis (a0, a1), Basis (a0, a1))
--  basis :: (a0, a1) -> ((CT a0, CT a1), (CT a0, CT a1))
--  basis (x0, x1) = ()

--instance HasBasis (Float, Float) where
----  type Cotangent (Float, Float) = (Cotangent Float, Cotangent Float)
--  -- type Basis (Float, Float) = ((Float, Float), (Float, Float))
--  basis :: (Float, Float) -> ((Float, Float), (Float, Float))
--  basis = const ((1, 0), (0, 1))

instance (HasBasis a0, HasBasis a1) => HasBasis (a0, a1) where
  basis1 :: forall b proxy.
    proxy (a0, a1) ->
    proxy b ->
    ((CT a0, CT a1) -> CT b) ->
    (T1 a0 (CT b), T1 a1 (CT b))
  basis1 _ _ f = (
      basis1 (Proxy @a0) (Proxy @b) (\ctx0 -> f (ctx0, zeroB (Proxy @a1))),
      basis1 (Proxy @a1) (Proxy @b) (\ctx1 -> f (zeroB (Proxy @a0), ctx1))
    )
--  basis1 :: forall b proxy.
--    proxy b ->
--    ((CT a0, CT a1) -> CT b) ->
--    (a0, a1) ->
--    (T1 a0 (CT b), T1 a1 (CT b))
--  basis1 _ f (x0, x1) = (
--      basis1 (Proxy @b) (\ctx0 -> f (ctx0, zeroB (Proxy @a1))) x0,
--      basis1 (Proxy @b) (\ctx1 -> f (zeroB (Proxy @a0), ctx1)) x1
--    )

--  oneB :: proxy (a0, a1) -> (CT a0, CT a1)
--  oneB _ = (zeroB (Proxy :: Proxy a0), zeroB (Proxy :: Proxy a1))

  zeroB :: proxy (a0, a1) -> (CT a0, CT a1)
  zeroB _ = (zeroB (Proxy :: Proxy a0), zeroB (Proxy :: Proxy a1))

--  zeroB1 :: forall b proxy. proxy (a0, a1) -> proxy b -> (Tangent1 a0 (Cotangent b), Tangent1 a1 (Cotangent b))
--  zeroB1 _ _ = (zeroB1 (Proxy :: Proxy a0) (Proxy :: Proxy b), zeroB1 (Proxy :: Proxy a1) (Proxy :: Proxy b))


--e0 :: forall cta0 a1 a2 proxy. (HasBasis a1, HasBasis a2) => cta0 -> proxy a1 -> proxy a2 -> (cta0, CT a1, CT a2)
--e0 x _ _ = (x, zeroB (Proxy :: Proxy a1), zeroB (Proxy :: Proxy a2))
--e1 :: forall a0 cta1 a2 proxy. (HasBasis a0, HasBasis a2) => proxy a0 -> cta1 -> proxy a2 -> (CT a0, cta1, CT a2)
--e1 _ x _ = (zeroB (Proxy :: Proxy a0), x, zeroB (Proxy :: Proxy a2))
--e2 :: forall a0 a1 cta2 proxy. (HasBasis a0, HasBasis a1) => proxy a0 -> proxy a1 -> cta2 -> (CT a0, CT a1, cta2)
--e2 _ _ x = (zeroB (Proxy :: Proxy a0), zeroB (Proxy :: Proxy a1), x)


instance (HasBasis a0, HasBasis a1, HasBasis a2) => HasBasis (a0, a1, a2) where
  basis1 :: forall b proxy. 
    proxy (a0, a1, a2) -> 
    proxy b -> 
    ((CT a0, CT a1, CT a2) -> CT b) -> 
    (T1 a0 (CT b), T1 a1 (CT b), T1 a2 (CT b))
  basis1 _ _ f = (
      basis1 (Proxy @a0) (Proxy @b) (\ctx0 -> f (ctx0, zeroB (Proxy @a1), zeroB (Proxy @a2))),
      basis1 (Proxy @a1) (Proxy @b) (\ctx1 -> f (zeroB (Proxy @a0), ctx1, zeroB (Proxy @a2))),
      basis1 (Proxy @a2) (Proxy @b) (\ctx2 -> f (zeroB (Proxy @a0), zeroB (Proxy @a1), ctx2))
    )
--  basis1 :: forall b proxy. 
--    proxy b -> 
--    ((CT a0, CT a1, CT a2) -> CT b) -> 
--    (a0, a1, a2) -> 
--    (T1 a0 (CT b), T1 a1 (CT b), T1 a2 (CT b))
--  basis1 _ f (x0, x1, x2) = (
--      basis1 (Proxy @b) (\ctx0 -> f (ctx0, zeroB (Proxy @a1), zeroB (Proxy @a2))) x0,
--      basis1 (Proxy @b) (\ctx1 -> f (zeroB (Proxy @a0), ctx1, zeroB (Proxy @a2))) x1,
--      basis1 (Proxy @b) (\ctx2 -> f (zeroB (Proxy @a0), zeroB (Proxy @a1), ctx2)) x2
--    )

  zeroB :: proxy (a0, a1, a2) -> (CT a0, CT a1, CT a2)
  zeroB = const (zeroB (Proxy :: Proxy a0), zeroB (Proxy :: Proxy a1), zeroB (Proxy :: Proxy a2))


longVecBasis :: (DVG.Vector v a, KnownNat n) =>
  Finite n -> a -> a -> DVGS.Vector v n a
longVecBasis k zero' one' = DVGS.generate $ \l ->
  if k == l
    then one'
    else zero'


instance (KnownNat n, HasBasis a) => HasBasis (LongVec n a) where
  basis1 :: forall b proxy. proxy (LongVec n a) -> proxy b -> (LongVec n (CT a) -> CT b) -> LongVec n (T1 a (CT b))
  basis1 _ _ f = DVGS.generate $ \k -> basis1 (Proxy @a) (Proxy @b) (f . longVecBasis k (zeroB (Proxy @a)))
--  basis1 :: forall b proxy. proxy b -> (LongVec n (CT a) -> CT b) -> LongVec n a -> LongVec n (T1 a (CT b))
--  basis1 _ f v = DVGS.generate $ \k -> basis1 (Proxy @b) (f . longVecBasis k (zeroB (Proxy @a))) (DVGS.index v k)

  zeroB :: proxy (LongVec n a) -> LongVec n (CT a)
  zeroB _ = DVGS.replicate (zeroB (Proxy :: Proxy a))


--instance HasBasis (DVFB.Vec n Float) where
----  type Cotangent (DVFB.Vec n Float) = DVFB.Vec n Float
--  -- type Basis (DVFB.Vec n Float) = (DVFB.Vec n (DVFB.Vec n Float))
--  basis :: DVFB.Vec n Float -> DVFB.Vec n (DVFB.Vec n Float)
--  basis = const undefined

--instance HasBasis (DVFB.Vec 2 Float, DVFB.Vec 3 Float) where
--  type Cotangent (DVFB.Vec 2 Float, DVFB.Vec 3 Float) = (DVFB.Vec 2 Float, DVFB.Vec 3 Float)
--  type Basis (DVFB.Vec 2 Float, DVFB.Vec 3 Float) = (
--      DVFB.Vec 2 (DVFB.Vec 2 Float, DVFB.Vec 3 Float),
--      DVFB.Vec 3 (DVFB.Vec 2 Float, DVFB.Vec 3 Float)
--    )
--  basis :: (DVFB.Vec 2 Float, DVFB.Vec 3 Float) -> (
--     DVFB.Vec 2 (DVFB.Vec 2 Float, DVFB.Vec 3 Float),
--     DVFB.Vec 3 (DVFB.Vec 2 Float, DVFB.Vec 3 Float)
--   )
--  basis = const undefined

sBasis :: Natural -> a -> a -> BoundedStream a
--sBasis n zero' one' = case n of 
--  0 -> emptyFiniteList
--  1 -> unit one'
--  _ -> bJoin zero' (basis (n - 1))
sBasis n zero' one' = basisStream zero' one' !! fromIntegral n

instance (HasBasis a) => HasBasis (Stream a) where
  basis1 :: forall b proxy. proxy (Stream a) -> proxy b -> (BoundedStream (CT a) -> CT b) -> Stream (T1 a (CT b))
  basis1 _ _ f = map
    (\n -> basis1 (Proxy @a) (Proxy @b) (\x -> f (boundedStreamBasis (zeroB (Proxy @a)) x n)))
    (fromList [0 ..])
--  basis1 :: forall b proxy. proxy b -> (BoundedStream (CT a) -> CT b) -> Stream a -> Stream (T1 a (CT b))
--  basis1 _ f s = map
--    (\(n, x') -> basis1 (Proxy @b) (\x -> f (boundedStreamBasis (zeroB (Proxy @a)) x n)) x')
--    (Stream.zip (fromList [0 ..]) s)
  zeroB :: proxy (Stream a) -> BoundedStream (CT a)
  zeroB _ = unitBoundedStream $ zeroB (Proxy @a)

instance (HasBasis a) => HasBasis (BoundedStream a) where
--  basis1 :: forall b proxy. proxy b -> (Stream (CT a) -> CT b) -> BoundedStream a -> BoundedStream (T1 a (CT b))
--  basis1 _ f bs = undefined -- BoundedStream $ fromList $ fmap (basis1   ) ()
  zeroB :: proxy (BoundedStream a) -> Stream (CT a)
  zeroB _ = repeat $ zeroB (Proxy @a)





















