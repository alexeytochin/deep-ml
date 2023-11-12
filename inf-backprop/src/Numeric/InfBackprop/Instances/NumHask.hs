{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module    :  Numeric.InfBackprop.Instances.NumHask
-- Copyright   :  (C) 2025 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Orphane instances for
-- [numhask](https://hackage.haskell.org/package/numhask)
-- typeclasses.
module Numeric.InfBackprop.Instances.NumHask () where

{- HLINT ignore "Use fewer imports" -}

import Control.Applicative (liftA2)
import Data.Bifunctor (bimap)
import qualified Data.Stream as DS
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Generic.Sized as DVGS
import GHC.Base (Functor (fmap), Maybe (Just))
import GHC.TypeNats (KnownNat)
import NumHask
  ( Additive,
    Divisive,
    ExpField,
    Multiplicative,
    Subtractive,
    TrigField,
    acos,
    acosh,
    asin,
    asinh,
    atan,
    atan2,
    atanh,
    cos,
    cosh,
    exp,
    log,
    logBase,
    negate,
    one,
    pi,
    recip,
    sin,
    sinh,
    sqrt,
    tan,
    tanh,
    zero,
    (*),
    (**),
    (+),
    (-),
    (/),
  )
import Numeric.InfBackprop.Utils.Tuple (biCross, biCross3, cross, cross3)

-- | Version-specific imports and instances.
-- These are needed for compatibility with different numhask versions.
#if MIN_VERSION_numhask(0,11,0)
#else
import NumHask (Distributive, Field, Ring)
#endif

-- | Instances for NumHask classes for common data types.
-- These instances follow the standard lifting of operations to container types.
--
-- Note: These are orphan instances. Consider proposing them upstream to numhask.

-- | Tuple instance of `Additive` typecalss.
instance
  (Additive a0, Additive a1) =>
  Additive (a0, a1)
  where
  zero = (zero, zero)
  (+) = biCross (+) (+)

-- | Tuple instance of `Subtractive` typeclass.
instance
  (Subtractive a0, Subtractive a1) =>
  Subtractive (a0, a1)
  where
  negate (x0, x1) = (negate x0, negate x1)
  (-) = biCross (-) (-)

-- | Tuple instance of `Multiplicative` typeclass.
instance
  (Multiplicative a0, Multiplicative a1) =>
  Multiplicative (a0, a1)
  where
  one = (one, one)
  (*) = biCross (*) (*)

-- | Tuple instance of `Divisive` typeclass.
instance
  (Divisive a0, Divisive a1) =>
  Divisive (a0, a1)
  where
  recip = cross recip recip
  (/) = biCross (/) (/)

#if MIN_VERSION_numhask(0,11,0)
#else
instance (Distributive a0, Distributive a1) => Distributive (a0, a1)
instance (Ring a0, Ring a1) => Ring (a0, a1)
instance (Field a0, Field a1) => Field (a0, a1)
#endif

-- | Tuple instance of `ExpField` typeclass.
instance
  (ExpField a, ExpField b) =>
  ExpField (a, b)
  where
  exp = bimap exp exp
  log = bimap log log
  (**) = biCross (**) (**)
  logBase = biCross logBase logBase
  sqrt = bimap sqrt sqrt

-- | Tuple instance of `TrigField` typeclass.
instance
  (TrigField a, TrigField b) =>
  TrigField (a, b)
  where
  -- Constants
  pi = (pi, pi)

  -- Basic trig functions
  sin = bimap sin sin
  cos = bimap cos cos
  tan = bimap tan tan

  -- Inverse trig functions
  asin = bimap asin asin
  acos = bimap acos acos
  atan = bimap atan atan
  atan2 = biCross atan2 atan2

  -- Hyperbolic functions
  sinh = bimap sinh sinh
  cosh = bimap cosh cosh
  tanh = bimap tanh tanh

  -- Inverse hyperbolic functions
  asinh = bimap asinh asinh
  acosh = bimap acosh acosh
  atanh = bimap atanh atanh

-- | Triple instance of `Additive`.
instance
  (Additive a0, Additive a1, Additive a2) =>
  Additive (a0, a1, a2)
  where
  zero = (zero, zero, zero)
  (+) = biCross3 (+) (+) (+)

-- | Triple instance of `Subtractive`.
instance
  (Subtractive a0, Subtractive a1, Subtractive a2) =>
  Subtractive (a0, a1, a2)
  where
  negate (x0, x1, x2) = (negate x0, negate x1, negate x2)
  (-) = biCross3 (-) (-) (-)

-- | Triple instance of `Multiplicative` typeclass.
instance
  (Multiplicative a0, Multiplicative a1, Multiplicative a2) =>
  Multiplicative (a0, a1, a2)
  where
  one = (one, one, one)
  (*) = biCross3 (*) (*) (*)

-- | Triple instance of `Divisive` typeclass.
instance
  (Divisive a0, Divisive a1, Divisive a2) =>
  Divisive (a0, a1, a2)
  where
  recip = cross3 recip recip recip
  (/) = biCross3 (/) (/) (/)

#if MIN_VERSION_numhask(0,11,0)
#else
instance (Distributive a0, Distributive a1, Distributive a2) => 
  Distributive (a0, a1, a2)
instance (Ring a0, Ring a1, Ring a2) => Ring (a0, a1, a2)
instance (Field a0, Field a1, Field a2) => Field (a0, a1, a2)
#endif

-- | Triple instance of `ExpField`.
instance
  (ExpField a0, ExpField a1, ExpField a2) =>
  ExpField (a0, a1, a2)
  where
  exp = cross3 exp exp exp
  log = cross3 log log log
  (**) = biCross3 (**) (**) (**)
  logBase = biCross3 logBase logBase logBase
  sqrt = cross3 sqrt sqrt sqrt

-- | Triple instance of `TrigField`.
instance
  (TrigField a, TrigField b, TrigField c) =>
  TrigField (a, b, c)
  where
  -- Constants
  pi = (pi, pi, pi)

  -- Basic trig functions
  sin = cross3 sin sin sin
  cos = cross3 cos cos cos
  tan = cross3 tan tan tan

  -- Inverse trig functions
  asin = cross3 asin asin asin
  acos = cross3 acos acos acos
  atan = cross3 atan atan atan
  atan2 = biCross3 atan2 atan2 atan2

  -- Hyperbolic functions
  sinh = cross3 sinh sinh sinh
  cosh = cross3 cosh cosh cosh
  tanh = cross3 tanh tanh tanh

  -- Inverse hyperbolic functions
  asinh = cross3 asinh asinh asinh
  acosh = cross3 acosh acosh acosh
  atanh = cross3 atanh atanh atanh

-- | Sized Vector instance of `Additive` typeclass.
instance
  (KnownNat n, Additive a, DVG.Vector v a) =>
  Additive (DVGS.Vector v n a)
  where
  zero = DVGS.replicate zero
  (+) = DVGS.zipWith (+)

-- | Sized Vector instance of `Subtractive` typeclass.
instance
  (KnownNat n, Subtractive a, DVG.Vector v a) =>
  Subtractive (DVGS.Vector v n a)
  where
  negate = DVGS.map zero
  (-) = DVGS.zipWith (-)

-- | Sized Vector instance of `Multiplicative` typeclass.
instance
  (KnownNat n, Multiplicative a, DVG.Vector v a) =>
  Multiplicative (DVGS.Vector v n a)
  where
  one = DVGS.replicate one
  (*) = DVGS.zipWith (*)

-- | Sized Vector instance of `Divisive` typeclass.
instance
  (KnownNat n, Divisive a, DVG.Vector v a) =>
  Divisive (DVGS.Vector v n a)
  where
  (/) = DVGS.zipWith (/)

#if MIN_VERSION_numhask(0,11,0)
#else
instance (KnownNat n, Distributive a, DVG.Vector v a) => Distributive (DVGS.Vector v n a)
instance (KnownNat n, Ring a, DVG.Vector v a) => Ring (DVGS.Vector v n a) where
instance (KnownNat n, Field a, DVG.Vector v a) => Field (DVGS.Vector v n a) where
#endif

-- | Sized Vector instance of `ExpField` typeclass.
instance
  (KnownNat n, ExpField a, DVG.Vector v a) =>
  ExpField (DVGS.Vector v n a)
  where
  exp = DVGS.map exp
  log = DVGS.map log
  (**) = DVGS.zipWith (**)
  logBase = DVGS.zipWith logBase
  sqrt = DVGS.map sqrt

-- | Sized Vector instance of `TrigField` typeclass.
instance
  (KnownNat n, TrigField a, DVG.Vector v a) =>
  TrigField (DVGS.Vector v n a)
  where
  -- Constants
  pi = DVGS.replicate pi

  -- Basic trig functions
  sin = DVGS.map sin
  cos = DVGS.map cos
  tan = DVGS.map tan

  -- Inverse trig functions
  asin = DVGS.map asin
  acos = DVGS.map acos
  atan = DVGS.map atan
  atan2 = DVGS.zipWith atan2

  -- Hyperbolic functions
  sinh = DVGS.map sinh
  cosh = DVGS.map cosh
  tanh = DVGS.map tanh

  -- Inverse hyperbolic functions
  asinh = DVGS.map asinh
  acosh = DVGS.map acosh
  atanh = DVGS.map atanh

-- | `Data.Stream.Stream` instances  of `Additive` typeclass.
instance
  (Additive a) =>
  Additive (DS.Stream a)
  where
  zero = DS.repeat zero
  (+) = DS.zipWith (+)

-- | `Data.Stream.Stream` instances  of `Subtractive` typeclass.
instance
  (Subtractive a) =>
  Subtractive (DS.Stream a)
  where
  negate = fmap negate
  (-) = DS.zipWith (-)

-- | `Data.Stream.Stream` instances  of `Multiplicative` typeclass.
instance
  (Multiplicative a) =>
  Multiplicative (DS.Stream a)
  where
  one = DS.repeat one
  (*) = liftA2 (*)

-- | `Data.Stream.Stream` instances  of `Divisive` typeclass.
instance
  (Divisive a) =>
  Divisive (DS.Stream a)
  where
  recip = fmap recip
  (/) = liftA2 (/)

#if MIN_VERSION_numhask(0,11,0)
#else
instance Maybe a => Distributive (DS.Stream a)
instance Ring a => Ring (DS.Stream a) where
instance Field a => Field (DS.Stream a) where
#endif

-- | `Data.Stream.Stream` instances  of `ExpField` typeclass.
instance
  (ExpField a) =>
  ExpField (DS.Stream a)
  where
  exp = fmap exp
  log = fmap log
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sqrt = fmap sqrt

-- | `Data.Stream.Stream` instances  of `TrigField` typeclass.
instance
  (TrigField a) =>
  TrigField (DS.Stream a)
  where
  -- Constants
  pi = DS.repeat pi

  -- Basic trig functions
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan

  -- Inverse trig functions
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  atan2 = liftA2 atan2

  -- Hyperbolic functions
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh

  -- Inverse hyperbolic functions
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

-- | `Maybe` instance of `Additive`.
instance
  (Additive a) =>
  Additive (Maybe a)
  where
  zero = Just zero
  (+) = liftA2 (+)

-- | `Maybe` instance of `Subtractive`.
instance
  (Subtractive a) =>
  Subtractive (Maybe a)
  where
  negate = fmap negate
  (-) = liftA2 (-)

-- | `Maybe` instance of `Multiplicative`.
instance
  (Multiplicative a) =>
  Multiplicative (Maybe a)
  where
  one = Just one
  (*) = liftA2 (*)

-- | `Maybe` instance of `Divisive`.
instance
  (Divisive a) =>
  Divisive (Maybe a)
  where
  recip = fmap recip
  (/) = liftA2 (/)

#if MIN_VERSION_numhask(0,11,0)
#else
instance Maybe a => Distributive (Maybe a)
instance Ring a => Ring (Maybe a)
instance Field a => Field (Maybe a)
#endif

-- | `Maybe` instance of `ExpField`.
instance
  (ExpField a) =>
  ExpField (Maybe a)
  where
  exp = fmap exp
  log = fmap log
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sqrt = fmap sqrt

-- | `Maybe` instance of `TrigField`.
instance
  (TrigField a) =>
  TrigField (Maybe a)
  where
  -- Constants
  pi = Just pi

  -- Basic trig functions
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan

  -- Inverse trig functions
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  atan2 = liftA2 atan2

  -- Hyperbolic functions
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh

  -- Inverse hyperbolic functions
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh
