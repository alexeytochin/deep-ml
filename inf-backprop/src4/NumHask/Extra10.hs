--{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module    :  NumHask.Extra
-- Copyright   :  (C) 2024 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Additional orphan instances for
-- [mumhusk](https://hackage.haskell.org/package/numhask)
-- typeclasses.
module NumHask.Extra10 where

import NumHask (Additive, zero, (+), Multiplicative, (*), one)
import Prelude hiding (Num, (+), (*))
import GHC.TypeNats (Nat, KnownNat)
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector as DV
import qualified Data.Vector.Fixed.Boxed as DVFB
import qualified Data.Vector.Fixed as DVF

type LongVec (n :: Nat) a =
 DVGS.Vector DV.Vector n a

--instance {-# INCOHERENT #-} Additive () where
instance Additive () where
  (+) = const
  zero = ()

--instance {-# INCOHERENT #-} (Additive x, Additive y) => Additive (x, y) where
instance (Additive x, Additive y) => Additive (x, y) where
  zero = (zero, zero)
  (a, b) + (c, d) = (a + c, b + d)

--instance {-# INCOHERENT #-} (Additive x, Additive y, Additive z) => Additive (x, y, z) where
instance (Additive x, Additive y, Additive z) => Additive (x, y, z) where
  zero = (zero, zero, zero)
  (x1, y1, z1) + (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

--instance {-# INCOHERENT #-} (Additive x, Additive y, Additive z, Additive t) => Additive (x, y, z, t) where
instance (Additive x, Additive y, Additive z, Additive t) => Additive (x, y, z, t) where
  zero = (zero, zero, zero, zero)
  (x1, y1, z1, t1) + (x2, y2, z2, t2) = (x1 + x2, y1 + y2, z1 + z2, t1 + t2)

instance
--  {-# INCOHERENT #-}
  (Additive x, Additive y, Additive z, Additive t, Additive s) =>
  Additive (x, y, z, t, s)
  where
  zero = (zero, zero, zero, zero, zero)
  (x1, y1, z1, t1, s1) + (x2, y2, z2, t2, s2) = (x1 + x2, y1 + y2, z1 + z2, t1 + t2, s1 + s2)

instance (Additive x, KnownNat n) =>
  Additive (LongVec n x) where
    zero = DVGS.replicate zero
    (+) = DVGS.zipWith (+)

instance (Multiplicative x, KnownNat n) =>
  Multiplicative (LongVec n x) where
    one = DVGS.replicate one
    (*) = DVGS.zipWith (*)