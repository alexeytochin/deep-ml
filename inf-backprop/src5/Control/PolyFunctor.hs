{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-export-lists #-}


module Control.PolyFunctor where


import Prelude (id, Functor, fmap, (.))
import GHC.Base (Type)


class Vectorizable (a :: Type) (b :: Type) (c :: Type) (d :: Type) where -- | a b c -> d, a b d -> c where
  ffmap :: (a -> b) -> (c -> d)

instance Vectorizable a b a b where
  ffmap = id

instance (Functor f, Vectorizable a b c d) =>
  Vectorizable a b (f c) (f d) where
    ffmap = fmap . ffmap

--instance (Vectorizable a b c0 d0, Vectorizable a b c1 d1) =>
--  Vectorizable a b (c0, c1) (d0, d1) where
--    ffmap f = cross (ffmap f) (ffmap f)

--instance Vectorizable a b (a, a) (b, b) where
--  ffmap f (x, y) = (f x, f y)

class BinnaryVectorizable (a :: Type) (b :: Type) (c :: Type) (d :: Type) (e :: Type) (f :: Type) where
  binarryFfmap :: (a -> b -> c) -> (d -> e -> f)

instance BinnaryVectorizable a b c a b c where
  binarryFfmap = id

instance (BinnaryVectorizable a b c d0 e0 f0, BinnaryVectorizable a b c d1 e1 f1) =>
  BinnaryVectorizable a b c (d0, d1) (e0, e1) (f0, f1) where
    binarryFfmap f (x0, x1) (y0, y1) = (binarryFfmap f x0 y0, binarryFfmap f x1 y1)



class Vectorizable1 (a :: Type) (b :: Type) where
  ffmap1 :: (a -> a) -> (b -> b)

instance Vectorizable1 a a where
  ffmap1 = id

instance (Functor f, Vectorizable1 a b) =>
  Vectorizable1 a (f b) where
    ffmap1 = fmap . ffmap1


class BinnaryVectorizable1 (a :: Type) (b :: Type) where
  binarryFfmap1 :: (a -> a -> a) -> (b -> b -> b)

instance BinnaryVectorizable1 a a where
  binarryFfmap1 = id

--instance (Functor f, BinnaryVectorizable1 a b) =>
--  Vectorizable1 a (f b) where
--    binarryFfmap1 = fmap . binarryFfmap1

instance (BinnaryVectorizable1 a b0, BinnaryVectorizable1 a b1) =>
  BinnaryVectorizable1 a (b0, b1) where
    binarryFfmap1 f (x0, x1) (y0, y1) = (binarryFfmap1 f x0 y0, binarryFfmap1 f x1 y1)

--class Vectorizable a a b b => Vectorizable2 a b where
--  ffmap2 :: (a -> a) -> (b -> b)
--  ffmap2 = ffmap

-- instance Vectorizable2 a a

-- instance Vectorizable2 a (a, a)
