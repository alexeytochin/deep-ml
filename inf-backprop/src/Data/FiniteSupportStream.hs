{-# LANGUAGE DeriveFoldable #-}

-- | Module    :  Data.FiniteSupportStream
-- Copyright   :  (C) 2025 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- This module provides functionality for working with infinite streams that have
-- finite support (i.e., only finitely many non-zero elements). The streams are
-- internally represented as arrays for efficient computation.
--
-- Any linear functional on an ordinary stream ('Data.Stream.Stream')
-- can be represented as a finite support stream.
-- Inversely, any finite support stream can be represented as
-- a linear functional on an ordinary stream.
module Data.FiniteSupportStream
  ( -- * The type of finite support streams
    FiniteSupportStream (toVector, MkFiniteSupportStream),

    -- * Basic functions
    supportLength,
    null,
    head,
    tail,
    cons,
    cons',
    streamsConvolution,
    finiteSupportStreamSum,
    unsafeMap,

    -- * Transformations
    optimize,
    unsafeZip,
    unsafeZipWith,

    -- * Construction
    mkFiniteSupportStream',
    empty,
    singleton,
    singleton',
    replicate,
    replicate',
    unsafeFromList,
    fromTuple,
    finiteSupportStreamBasis,

    -- * Conversion
    multiplicativeAction,
    takeArray,
    takeList,
    toList,
    toInfiniteList,

    -- * Fold tools
    foldlWithStream,
    foldlWithStream',
  )
where

import Control.ExtendableMap (ExtandableMap, extendMap)
import Data.Eq ((==))
import Data.Foldable (Foldable, foldl')
import qualified Data.IndexedListLiterals as DILL
import Data.List (repeat, (++))
import qualified Data.List as DL
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import qualified Data.Stream as DS
import Data.Tuple (fst)
import Data.Vector (Vector)
import qualified Data.Vector as DV
import Debug.SimpleExpr.Utils.Algebra
  ( AlgebraicPower ((^^)),
    Convolution ((|*|)),
    MultiplicativeAction ((*|)),
  )
import GHC.Base (Bool, Eq, fmap, id, ($), (.), (<>), (>))
import GHC.Natural (Natural)
import GHC.Real (fromIntegral)
import GHC.Show (Show, show)
import NumHask
  ( Additive,
    Distributive,
    Multiplicative,
    Subtractive,
    negate,
    zero,
    (*),
    (+),
    (-),
  )
import qualified NumHask
import Numeric.InfBackprop.Instances.NumHask ()
import Numeric.InfBackprop.Utils.Vector (safeHead, trimArrayTail)
import qualified Numeric.InfBackprop.Utils.Vector as DVIBP

-- | A stream with finite support, represented as a vector.
-- Elements beyond the vector's length are implicitly zero.
-- The vector may contain trailing zeros, which can be removed using 'optimize'.
--
-- The type parameter @a@ typically has an 'Additive' instance with a zero element.
--
-- ==== __Examples__
--
-- >>> import GHC.Base (Int, Float, Bool(False, True))
-- >>> import Data.Vector (fromList)
--
-- >>> MkFiniteSupportStream $ fromList [0, 1, 2, 3] :: FiniteSupportStream Int
-- [0,1,2,3,0,0,0,...
--
-- >>> MkFiniteSupportStream $ fromList [0, 1, 2, 3] :: FiniteSupportStream Float
-- [0.0,1.0,2.0,3.0,0.0,0.0,0.0,...
--
-- >>> MkFiniteSupportStream $ fromList [False, True] :: FiniteSupportStream Bool
-- [False,True,False,False,False,...
newtype FiniteSupportStream a = MkFiniteSupportStream {toVector :: DV.Vector a}
  deriving (Foldable)

-- | Lifts a function to work with finite support streams.
-- This function applies the provided function to each element of the stream support.
-- The function is usafe because it is not checked that the argument function
-- maps zero to zero, which is expected.
--
-- ==== __Examples__
--
-- >>> unsafeMap (*2) (MkFiniteSupportStream $ DV.fromList [0, 1, 2, 3])
-- [0,2,4,6,0,0,0,...
--
-- >>> unsafeMap (+1) (MkFiniteSupportStream $ DV.fromList [0, 1, 2, 3])
-- [1,2,3,4,0,0,0,...
unsafeMap :: (a -> b) -> FiniteSupportStream a -> FiniteSupportStream b
unsafeMap f (MkFiniteSupportStream array') = MkFiniteSupportStream $ DV.map f array'

-- | `Eq` instance of `FiniteSupportStream`.
instance (Eq a, Additive a) => Eq (FiniteSupportStream a) where
  x == y = x' == y'
    where
      x' = toVector $ optimize x
      y' = toVector $ optimize y

-- | `Show` instance of `FiniteSupportStream`.
instance forall a. (Show a, Eq a, Additive a) => Show (FiniteSupportStream a) where
  show bs =
    let (MkFiniteSupportStream array') = optimize bs
     in "["
          <> mconcat (fmap (\x -> show x <> ",") (DV.toList array' ++ [zero, zero, zero]))
          <> "..."

-- | `Additive` instance for `FiniteSupportStream`.
--
-- ==== __Examples__
--
-- >>> import GHC.Base (Int)
--
-- >>> (unsafeFromList [1, 2, 3]) + (unsafeFromList [10, 20]) :: FiniteSupportStream Int
-- [11,22,3,0,0,0,...
--
-- >>> (unsafeFromList [1, 2, 3]) + empty
-- [1,2,3,0,0,0,...
instance (Additive a) => Additive (FiniteSupportStream a) where
  zero = empty
  (MkFiniteSupportStream a0) + (MkFiniteSupportStream a1) =
    MkFiniteSupportStream $
      DVIBP.zipWith (+) id id a0 a1

-- | `Subtractive` instance for `FiniteSupportStream`.
--
-- ==== __Examples__
--
-- >>> unsafeFromList [10, 20, 30] - unsafeFromList [1, 2]
-- [9,18,30,0,0,0,...
--
-- >>> unsafeFromList [1, 2, 3] - unsafeFromList [1, 2, 3]
-- [0,0,0,...
instance (Subtractive a) => Subtractive (FiniteSupportStream a) where
  negate = unsafeMap negate
  (MkFiniteSupportStream a0) - (MkFiniteSupportStream a1) =
    MkFiniteSupportStream $
      DVIBP.zipWith (-) id negate a0 a1

-- | `FiniteSupportStream` instance of `MultiplicativeAction`.
instance
  (MultiplicativeAction a b) =>
  MultiplicativeAction a (FiniteSupportStream b)
  where
  (*|) = unsafeMap . (*|)

-- | `FiniteSupportStream`instance of `AlgebraicPower` typeclass
-- for raising `FiniteSupportStream` to powers.
instance
  (AlgebraicPower b a) =>
  AlgebraicPower b (FiniteSupportStream a)
  where
  x ^^ n = unsafeMap (^^ n) x

-- | `FiniteSupportStream` instance of 'ExtandableMap' typeclass.
instance
  (ExtandableMap a b c d) =>
  ExtandableMap a b (FiniteSupportStream c) (FiniteSupportStream d)
  where
  extendMap = unsafeMap . extendMap

-- | `Convolution` instance for `FiniteSupportStream` and `Data.Stream.Stream`.
instance
  (Convolution a b c, Additive c) =>
  Convolution (FiniteSupportStream a) (DS.Stream b) c
  where
  fss |*| s = foldlWithStream' (\acc x y -> acc + x |*| y) zero fss s

-- | `Convolution` instance for `Data.Stream.Stream` and `FiniteSupportStream`.
instance
  (Convolution a b c, Additive c) =>
  Convolution (DS.Stream a) (FiniteSupportStream b) c
  where
  s |*| fss = foldlWithStream' (\acc x y -> acc + y |*| x) zero fss s

-- | `Convolution` instance for `FiniteSupportStream` and `FiniteSupportStream`.
instance
  (Convolution a b c, Additive c) =>
  Convolution (FiniteSupportStream a) (FiniteSupportStream b) c
  where
  (MkFiniteSupportStream vx) |*| (MkFiniteSupportStream vy) = vx |*| vy

-- | Creates a finite support stream from a array, removing trailing zeros in the tail.
-- This is a constructor that ensures the minimal representation.
--
-- ==== __Examples__
--
-- >>> import Data.Vector (fromList)
--
-- >>> toVector $ mkFiniteSupportStream' $ fromList [0, 1, 2, 3, 0]
-- [0,1,2,3]
mkFiniteSupportStream' :: (Eq a, Additive a) => DV.Vector a -> FiniteSupportStream a
mkFiniteSupportStream' array' = MkFiniteSupportStream $ trimArrayTail zero array'

-- | Removes trailing elements of the finite support stream's inner array
-- if they are zeros.
-- The resulting stream is represented in its minimal form.
--
-- ==== __Examples__
--
-- >>> optimize $ unsafeFromList [0, 1, 0, 3, 0, 0]
-- [0,1,0,3,0,0,0,...
optimize :: (Eq a, Additive a) => FiniteSupportStream a -> FiniteSupportStream a
optimize (MkFiniteSupportStream array') = mkFiniteSupportStream' array'

-- | Returns the length of the stream's support (the vector length after optimization).
-- Trailing zeros are not counted in the support length.
--
-- ==== __Examples__
--
-- >>> import GHC.Base (Int)
--
-- >>> supportLength $ unsafeFromList [0, 1, 2, 3]
-- 4
--
-- >>> supportLength $ unsafeFromList [0, 1, 2, 3, 0, 0]
-- 6
supportLength :: FiniteSupportStream a -> Natural
supportLength = fromIntegral . DV.length . toVector

-- | Checks if the finite support stream is empty.
--
-- ==== __Examples__
--
-- >>> null $ unsafeFromList [0, 1, 2]
-- False
--
-- >>> null $ unsafeFromList []
-- True
--
-- >>> null $ unsafeFromList [0, 0, 0]
-- False
null :: FiniteSupportStream a -> Bool
null = DV.null . toVector

-- | Converts a finite list to a 'FiniteSupportStream'.
-- The list is assumed to be finite.
-- Trailing zero elements are not checked, and the inner array is not trimmed.
--
-- ==== __Examples__
--
-- >>> import GHC.Base (Int, Float, Bool(False, True))
--
-- >>> unsafeFromList [0, 1, 2, 3] :: FiniteSupportStream Int
-- [0,1,2,3,0,0,0,...
--
-- >>> unsafeFromList [0, 1, 2, 3] :: FiniteSupportStream Float
-- [0.0,1.0,2.0,3.0,0.0,0.0,0.0,...
--
-- >>> unsafeFromList [False, True]
-- [False,True,False,False,False,...
unsafeFromList :: [a] -> FiniteSupportStream a
unsafeFromList = MkFiniteSupportStream . DV.fromList

-- | Converts a tuple into a `FiniteSupportStream`.
-- Trailing zero elements are not checked, and the inner array is not trimmed.
--
-- === __Examples__
--
-- >>> import GHC.Base (Int, Float, Bool(False, True))
-- >>> import GHC.Integer (Integer)
--
-- >>> fromTuple (0, 1, 2, 3) :: FiniteSupportStream Integer
-- [0,1,2,3,0,0,0,...
--
-- >>> fromTuple (0 :: Float, 1 :: Float, 2 :: Float, 3 :: Float) :: FiniteSupportStream Float
-- [0.0,1.0,2.0,3.0,0.0,0.0,0.0,...
--
-- >>> fromTuple (False, True) :: FiniteSupportStream Bool
-- [False,True,False,False,False,...
fromTuple ::
  (DILL.IndexedListLiterals input length a) =>
  input ->
  FiniteSupportStream a
fromTuple = MkFiniteSupportStream . DV.fromList . DILL.toList

-- | Converts a finite support stream to a finite list.
-- The resulting list includes all elements of the stream, including any trailing zeros.
--
-- ==== __Examples__
--
-- >>> toList $ unsafeFromList [1, 2, 3]
-- [1,2,3]
--
-- >>> toList $ unsafeFromList [1, 2, 3, 0]
-- [1,2,3,0]
toList :: FiniteSupportStream a -> [a]
toList = DV.toList . toVector

-- | Converts a finite support stream to an infinite list.
-- The resulting list contains all elements of the stream, followed by an infinite sequence of zeros.
--
-- ==== __Examples__
--
-- >>> import Data.List (take)
--
-- >>> take 5 $ toInfiniteList $ unsafeFromList [1, 2, 3]
-- [1,2,3,0,0]
toInfiniteList :: (Additive a) => FiniteSupportStream a -> [a]
toInfiniteList fss = toList fss ++ repeat zero

-- | Empty finite support stream.
-- The stream contains only zeros.
--
-- ==== __Examples__
--
-- >>> import GHC.Base (Int, Bool)
--
-- >>> empty :: FiniteSupportStream Int
-- [0,0,0,...
--
-- >>> empty :: FiniteSupportStream Bool
-- [False,False,False,...
empty :: FiniteSupportStream a
empty = MkFiniteSupportStream DV.empty

-- | Returns the first element of the finite support stream.
-- If the stream is empty, it returns 'zero'.
--
-- ==== __Examples__
--
-- >>> import GHC.Base (Int, Bool)
--
-- >>> head $ unsafeFromList [1, 2, 3]
-- 1
--
-- >>> head $ empty :: Int
-- 0
--
-- >>> head $ empty :: Bool
-- False
head :: (Additive a) => FiniteSupportStream a -> a
head (MkFiniteSupportStream array') = fromMaybe zero (safeHead array')

-- | Removes the first element of the finite support stream.
-- If the stream is empty, it returns an empty stream.
--
-- ==== __Examples__
--
-- >>> import GHC.Base (Int)
--
-- >>> tail $ unsafeFromList [1, 2, 3]
-- [2,3,0,0,0,...
--
-- >>> tail $ empty :: FiniteSupportStream Int
-- [0,0,0,...
tail :: FiniteSupportStream a -> FiniteSupportStream a
tail (MkFiniteSupportStream array') =
  if DV.null array'
    then empty
    else MkFiniteSupportStream $ DV.tail array'

-- | Takes the first @n@ elements of the finite support stream in the form of an array.
-- If @n@ is greater than the length of the stream, the result is padded with zeros.
-- The resulting array is not trimmed.
--
-- ==== __Examples__
--
-- >>> takeArray 5 $ unsafeFromList [1, 2, 3]
-- [1,2,3,0,0]
takeArray :: (Additive a) => Natural -> FiniteSupportStream a -> Vector a
takeArray n (MkFiniteSupportStream array) =
  if fromIntegral n > DV.length array
    then array <> DV.replicate (fromIntegral n - DV.length array) zero
    else DV.slice 0 (fromIntegral n) array

-- | Takes the first @n@ elements of the finite support stream in the form of a list.
-- If @n@ is greater than the length of the stream, the result is padded with zeros.
--
-- ==== __Examples__
--
-- >>> takeList 5 $ unsafeFromList [1, 2, 3]
-- [1,2,3,0,0]
takeList :: (Additive a) => Natural -> FiniteSupportStream a -> [a]
takeList n fss = DV.toList $ takeArray n fss

-- | Creates a finite support stream with exactly one element.
-- The element is not checked for being zero.
--
-- ==== __Examples__
--
-- >>> toVector $ singleton 42
-- [42]
--
-- >>> toVector $ singleton 0
-- [0]
--
-- >>> singleton 42
-- [42,0,0,0,...
--
-- >>> singleton 0
-- [0,0,0,...
--
-- >>> toVector $ singleton "a"
-- ["a"]
singleton :: a -> FiniteSupportStream a
singleton = MkFiniteSupportStream . DV.singleton

-- | Creates a finite support stream with exactly one non-zero element
-- if the provided element is not zero.
-- Returns the empty stream otherwise.
--
-- ==== __Examples__
--
-- >>> toVector $ singleton' 42
-- [42]
--
-- >>> toVector $ singleton' 0
-- []
--
-- >>> singleton' 42
-- [42,0,0,0,...
--
-- >>> singleton' 0
-- [0,0,0,...
singleton' :: (Additive a, Eq a) => a -> FiniteSupportStream a
singleton' x =
  if x == zero
    then empty
    else MkFiniteSupportStream $ DV.singleton x

-- | Creates a finite support stream with a constant value along the support.
-- It does not check whether the provided value is zero.
-- In this case, the inner array contains only zeros.
--
-- ==== __Examples__
--
-- >>> replicate 3 42
-- [42,42,42,0,0,0,...
--
-- >>> replicate 2 0
-- [0,0,0,...
--
-- >>> toVector $ replicate 2 0
-- [0,0]
replicate :: Natural -> a -> FiniteSupportStream a
replicate n x = MkFiniteSupportStream $ DV.replicate (fromIntegral n) x

-- | Creates a finite support stream with a constant value along the support.
-- It checks whether the provided value is zero.
-- In this case, the inner array is empty.
--
-- ==== __Examples__
--
-- >>> replicate' 3 42
-- [42,42,42,0,0,0,...
--
-- >>> replicate' 2 0
-- [0,0,0,...
--
-- >>> toVector $ replicate' 2 0
-- []
replicate' :: (Additive a, Eq a) => Natural -> a -> FiniteSupportStream a
replicate' n x =
  if x == zero
    then empty
    else MkFiniteSupportStream $ DV.replicate (fromIntegral n) x

-- | Adds an element to the front of the finite support stream.
-- The inner array size is increased by exactly one.
-- The head element of the array is not checked for zero elements.
--
-- ==== __Examples__
--
-- >>> cons 42 (unsafeFromList [1, 2, 3])
-- [42,1,2,3,0,0,0,...
--
-- >>> toVector $ cons 0 empty
-- [0]
cons :: a -> FiniteSupportStream a -> FiniteSupportStream a
cons x = MkFiniteSupportStream . DV.cons x . toVector

-- | Adds an element to the front of the finite support stream.
-- The inner array size is increased by exactly one if the head element is not zero.
-- Otherwise, if the finite support stream is empty, the output is also the empty stream.
--
-- ==== __Examples__
--
-- >>> cons' 42 (unsafeFromList [1, 2, 3])
-- [42,1,2,3,0,0,0,...
--
-- >>> toVector $ cons' 0 empty
-- []
cons' :: (Additive a, Eq a) => a -> FiniteSupportStream a -> FiniteSupportStream a
cons' x fss =
  let (MkFiniteSupportStream array') = optimize fss
   in if DV.null array'
        then singleton' x
        else MkFiniteSupportStream $ DV.cons x array'

-- | Creates a finite support stream basis vector.
-- The values of the zero and unit elements are provided as arguments.
--
-- ==== __Examples__
--
-- >>> finiteSupportStreamBasis 0 1 3
-- [0,0,0,1,0,0,0,...
finiteSupportStreamBasis :: a -> a -> Natural -> FiniteSupportStream a
finiteSupportStreamBasis zero' one' n =
  MkFiniteSupportStream $ DV.snoc (DV.replicate (fromIntegral n) zero') one'

-- | Convolves a stream with a finite support stream, producing a single value.
-- The result is the sum of element-wise products.
--
-- This operation is equivalent to applying the stream as a linear functional
-- to the finite support stream.
--
-- ==== __Examples__
--
-- >>> import GHC.Base (Float, Int)
-- >>> import GHC.Real ((/))
-- >>> import Data.Stream (iterate, take, Stream)
-- >>> import Data.HashMap.Internal.Array (fromList')
--
-- >>> s1 = iterate (+1) 0 :: Stream Int
-- >>> Data.Stream.take 5 s1
-- [0,1,2,3,4]
-- >>> fss1 = unsafeFromList [0, 0, 1] :: FiniteSupportStream Int
-- >>> streamsConvolution s1 fss1
-- 2
--
-- >>> s2 = iterate (/2) (1 :: Float) :: Stream Float
-- >>> Data.Stream.take 5 s2
-- [1.0,0.5,0.25,0.125,6.25e-2]
-- >>> fss2 = unsafeFromList $ Data.List.replicate 10 1 :: FiniteSupportStream Float
-- >>> streamsConvolution s2 fss2
-- 1.9980469
streamsConvolution ::
  (Distributive a) =>
  DS.Stream a ->
  FiniteSupportStream a ->
  a
streamsConvolution stream fss =
  foldl' (+) zero (DL.zipWith (*) (DS.toList stream) (toList fss))

-- | Applies the multiplicative action of the stream on the finite support stream.
-- The resulting stream's support length is less than or equal to
-- the stream's support length in the argument.
--
-- ==== __Examples__
--
-- >>> import GHC.Base (Int)
--
-- >>> multiplicativeAction (DS.fromList [0 ..]) (unsafeFromList [1, 1, 0, 1])
-- [0,1,0,3,0,0,0,...
multiplicativeAction ::
  (Multiplicative a) =>
  DS.Stream a ->
  FiniteSupportStream a ->
  FiniteSupportStream a
multiplicativeAction stream (MkFiniteSupportStream array) =
  MkFiniteSupportStream $
    DV.fromList $
      DL.zipWith (*) (DS.toList stream) (DV.toList array)

-- | Computes the sum of all elements the finite support stream.
--
-- ==== __Examples__
--
-- >>> import GHC.Base (Int)
--
-- >>> finiteSupportStreamSum $ unsafeFromList [1, 2, 3, 0] :: Int
-- 6
--
-- >>> finiteSupportStreamSum empty :: Int
-- 0
finiteSupportStreamSum :: (Additive a) => FiniteSupportStream a -> a
finiteSupportStreamSum (MkFiniteSupportStream array') = NumHask.sum array'

-- | Applies an element-wise binary operation to two streams.
--
-- Parameters:
--   * @f@ - Binary operation for overlapping elements
--   * @g@ - Unary operation for excess elements in first stream
--   * @h@ - Unary operation for excess elements in second stream
--
-- The resulting stream's length is the maximum of the input lengths,
-- with trailing elements transformed by @g@ or @h@ as appropriate.
--
-- ==== __Examples__
--
-- >>> import GHC.Base (Int)
--
-- >>> let xs = unsafeFromList [10, 20, 30]
-- >>> let ys = unsafeFromList [1,2]
-- >>> unsafeZipWith (-) id negate xs ys
-- [9,18,30,0,0,0,...
unsafeZipWith ::
  -- | Binary operation for overlapping elements
  (a -> b -> c) ->
  -- | Operation for excess elements in first stream
  (a -> c) ->
  -- | Operation for excess elements in second stream
  (b -> c) ->
  FiniteSupportStream a ->
  FiniteSupportStream b ->
  FiniteSupportStream c
unsafeZipWith f g h (MkFiniteSupportStream a0) (MkFiniteSupportStream a1) =
  MkFiniteSupportStream $ DVIBP.zipWith f g h a0 a1

-- | Lazy left fold over a foldable type @t@ and a `Data.Stream.Stream`.
--
-- ==== __Examples__
--
-- >>> foldlWithStream (\acc x y -> acc + x * y) 0 (unsafeFromList [1,1,1]) (DS.iterate (+1) 0)
-- 3
foldlWithStream ::
  (Foldable t) =>
  (b -> a -> c -> b) ->
  b ->
  t a ->
  DS.Stream c ->
  b
foldlWithStream f acc0 ta stream0 =
  fst $ foldl' step (acc0, stream0) ta
  where
    step (acc, DS.Cons c stream') a =
      (f acc a c, stream')

-- | Strinct left fold over a foldable type @t@ and a `Data.Stream.Stream`.
--
-- ==== __Examples__
--
-- >>> foldlWithStream (\acc x y -> acc + x * y) 0 (unsafeFromList [1,1,1]) (DS.iterate (+1) 0)
-- 3
foldlWithStream' ::
  (Foldable t) =>
  (b -> a -> c -> b) ->
  b ->
  t a ->
  DS.Stream c ->
  b
foldlWithStream' f !acc0 ta stream0 = fst $ foldl' step (acc0, stream0) ta
  where
    step (!acc, DS.Cons !c stream) a = let !acc' = f acc a c in (acc', stream)

-- | Zips two finite support streams.
--
-- ==== __Examples__
--
-- >>> import GHC.Base (Int)
--
-- >>> unsafeZip (unsafeFromList [1, 2, 3]) (unsafeFromList [4, 5]) :: FiniteSupportStream (Int, Int)
-- [(1,4),(2,5),(3,0),(0,0),(0,0),(0,0),...
unsafeZip ::
  (Additive a, Additive b) =>
  FiniteSupportStream a ->
  FiniteSupportStream b ->
  FiniteSupportStream (a, b)
-- {-# ANN module "HLint: ignore Use zip" #-}
unsafeZip = unsafeZipWith (,) (,zero) (zero,)
