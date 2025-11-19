-- |
-- Module    :  Numeric.InfBackprop.Utils.CachedIso
-- Copyright   :  (C) 2025 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Utility functions for working with sized vector.
module Numeric.InfBackprop.Utils.SizedVector
  ( BoxedVector,
    boxedVectorBasis,
    boxedVectorSum,
  )
where

import Data.Finite (Finite)
import qualified Data.Vector as DV
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Generic.Sized as DVGS
import GHC.Base (($), (==))
import GHC.TypeLits (Nat)
import GHC.TypeNats (KnownNat)
import NumHask (Additive, zero, (+))

-- | Type alias for boxed sized vectors.
type BoxedVector (n :: Nat) a = DVGS.Vector DV.Vector n a

-- | Creates a sized vector of size n with all elements set to @x :: a@
-- except for the one at index @k@, which is set to @y :: a@.
--
-- ==== __Examples__
--
-- >>> import GHC.Base (Int, String)
-- >>> import qualified Data.Vector as DV
-- >>> import qualified Data.Vector.Generic.Sized as DVGS
--
-- >>> boxedVectorBasis 2 0 1 :: DVGS.Vector DV.Vector 4 Int
-- Vector [0,0,1,0]
--
-- >>> boxedVectorBasis 1 "zero" "one" :: DVGS.Vector DV.Vector 5 String
-- Vector ["zero","one","zero","zero","zero"]
boxedVectorBasis ::
  (DVG.Vector v a, KnownNat n) =>
  Finite n ->
  a ->
  a ->
  DVGS.Vector v n a
boxedVectorBasis k zero' one' = DVGS.generate $ \l ->
  if k == l
    then one'
    else zero'

-- | Sums all elements of a sized array.
--
-- ==== __Examples__
--
-- >>> import GHC.Base (Int)
-- >>> import qualified Data.Vector as DV
-- >>> import qualified Data.Vector.Generic.Sized as DVGS
--
-- >>> boxedVectorSum (DVGS.fromTuple (1, 2, 3) :: DVGS.Vector DV.Vector 3 Int)
-- 6
boxedVectorSum :: (Additive a) => DVGS.Vector DV.Vector n a -> a
boxedVectorSum = DVGS.foldl' (+) zero
