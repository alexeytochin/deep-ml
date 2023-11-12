{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NumHask.SmallVectorOrphaneInstances () where

import Prelude (fmap)
import qualified Data.Vector.Fixed.Boxed as DVFB
import qualified Data.Vector.Fixed as DVF
import NumHask (
    Additive,
    (+),
    zero,
    Subtractive,
    (-),
    Multiplicative,
    (*),
    one,
    Divisive,
    (/), 
    AdditiveScalar, 
    AdditiveAction, 
    (|+), 
    Scalar,
    MultiplicativeAction,
    (|*), 
    DivisiveAction,
    (|/),
  )
#if MIN_VERSION_numhask(0,11,0)
#else
import NumHask (Distributive)
#endif

instance (DVF.Arity n, Additive a) => 
  Additive (DVFB.Vec n a) where
    (+) = DVF.zipWith (+)
    zero = DVF.replicate zero
    
instance (DVF.Arity n, Subtractive a) => 
  Subtractive (DVFB.Vec n a) where
    (-) = DVF.zipWith (-)

instance (DVF.Arity n, Multiplicative a) => 
  Multiplicative (DVFB.Vec n a) where
    (*) = DVF.zipWith (*)
    one = DVF.replicate one
    
#if MIN_VERSION_numhask(0,11,0)
#else
instance (DVF.Arity n, Distributive a) => 
  Distributive (DVFB.Vec n a)
#endif

instance (DVF.Arity n, Divisive a) => 
  Divisive (DVFB.Vec n a) where
    (/) = DVF.zipWith (/)
    
instance (DVF.Arity n, Additive (AdditiveScalar (DVFB.Vec n a))) =>
    AdditiveAction (DVFB.Vec n a) where
  type AdditiveScalar (DVFB.Vec n a) = a
  v |+ s = fmap (+s) v

instance (DVF.Arity n, Multiplicative (Scalar (DVFB.Vec n a))) =>
    MultiplicativeAction (DVFB.Vec n a) where
  type Scalar (DVFB.Vec n a) = a
  v |* s = fmap (*s) v
  
instance (DVF.Arity n, Divisive (Scalar (DVFB.Vec n a))) =>
    DivisiveAction (DVFB.Vec n a) where
  v |/ s = fmap (/s) v