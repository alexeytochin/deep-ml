{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE MagicHash             #-}

module Data.StreamExtra where

import qualified Data.HashMap.Internal.Array as Array
import Data.HashMap.Internal.Array (Array, new, new_, run, toList, write, map)
import Prelude (Show, Functor, show, (.), (<>), ($), (+), pure, fromIntegral, uncurry, 
  fmap, zip, mconcat, length)
import NumHask (Distributive, one, zero, (*), sum)
import qualified Data.Stream as Stream
import Data.Stream (Stream)
--import GHC.Exts            (Int (..), SmallArray#, SmallMutableArray#,
--                            cloneSmallMutableArray#, copySmallArray#,
--                            copySmallMutableArray#, indexSmallArray#,
--                            newSmallArray#, readSmallArray#,
--                            reallyUnsafePtrEquality#, sizeofSmallArray#,
--                            sizeofSmallMutableArray#, tagToEnum#,
--                            thawSmallArray#, unsafeCoerce#,
--                            unsafeFreezeSmallArray#, unsafeThawSmallArray#,
--                            writeSmallArray#)
import GHC.Natural (Natural)

--instance Foldable Array where
--  foldr :: (a -> b -> b) -> b -> Array a -> b
--  foldr f b (UnsafeMkFiniteList _ l) = P.foldr f b l



--newtype BoundedStream a = BoundedStream {getArray :: Array a} deriving
--  (Show, Foldable, Functor)

newtype BoundedStream a = BoundedStream {getArray :: Array a}

-- | Example
--
-- >>> unsafeFromList [0, 1, 2, 3]
-- 0, 1, 2, 3, 0, 0, 0, ...
instance Show a => Show (BoundedStream a) where
  show (BoundedStream array) = mconcat (fmap (\x -> show x <> ", ") (Array.toList array)) <> "0, 0, 0, ..." 

instance Functor BoundedStream where
  fmap f =  BoundedStream . map f . getArray

unsafeFromList :: [a] -> BoundedStream a
unsafeFromList lx = BoundedStream (Array.fromList (length lx) lx)

--instance Show a => Show (BoundedStream a) where
--  show s = show (toList s) --  <> ", (0)"

boundedStreamtoList :: BoundedStream a -> [a]
boundedStreamtoList (BoundedStream a) = toList a

emptyBoundedStream :: BoundedStream a
emptyBoundedStream = BoundedStream $ run $ new_ 0

-- | Example
-- 
-- >>> basis 3
-- 0, 0, 0, 1, 0, 0, 0, ...
basis :: Distributive a => Natural -> BoundedStream a
basis n = BoundedStream $ run $ do
  res <- new (nInt + 1) zero
  write res nInt one
  pure res where
    nInt = fromIntegral n

-- | Example
--
-- >>> import Prelude (Integer)
-- >>> import Data.Stream (iterate)
-- >>> import Data.HashMap.Internal.Array (fromList')
--
-- >>> stream = iterate (+1) 0 :: Stream Integer
-- >>> -- [0, 1, 2, 3, 4, ...]
-- >>> booundedStream = BoundedStream $ fromList' 3 [0, 0, 1] :: BoundedStream Integer
-- >>> convWithStream stream booundedStream
-- 2
convWithStream :: forall a. Distributive a => Stream a -> BoundedStream a -> a
convWithStream stream (BoundedStream array) =
  sum $ fmap (uncurry (*)) (zip (Stream.toList stream) (Array.toList array))
--convWithStream stream (BoundedStream array) = BoundedStream $ fst $ foldl' step (zero :: a, stream :: Stream a) array where
--  step (accum, Cons x tail) y = (accum + (x * y), tail)
   

--zipWithStream2 :: (a -> b -> c) -> Array a -> Stream b -> Array c
--zipWithStream2 f = flip $ zipWithStream (flip f)
--
--
--convWithStream :: Distributive a =>
--  Stream a -> Array a -> a
--convWithStream a b = sum $ zipWithStream (*) a b