-- | Module    :  Numeric.InfBackprop.Instances.NumHask
-- Copyright   :  (C) 2025 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- `ExtandableMap` class and its instances.
module Control.ExtendableMap
  ( ExtandableMap (extendMap),
  )
where

import Data.Bifunctor (bimap)
import Data.Stream (Stream)
import qualified Data.Vector as DV
import qualified Data.Vector.Generic.Sized as DVGS
import GHC.Base (fmap, id, (.))

-- | Type is similar to `fmap`, but it can extend the function application.
-- It can apply a function to each element and subelements
-- of a tuple, list, sized vector, stream, etc.
--
-- ==== __Examples__
--
-- >>> import GHC.Base (Bool(True, False), Int)
-- >>> import GHC.Show (show)
-- >>> import GHC.Num ((*))
-- >>> import Data.String (String)
--
-- >>> extendMap (show :: Int -> String) (42 :: Int) :: String
-- "42"
--
-- >>> extendMap (show :: Bool -> String) (True, False) :: (String, String)
-- ("True","False")
--
-- >>> extendMap ((*2) :: Int -> Int) (1 :: Int, (2 :: Int, 3 :: Int)) :: (Int, (Int, Int))
-- (2,(4,6))
--
-- >>> extendMap ((*2) :: Int -> Int) ([1, 2, 3] :: [Int], (4 :: Int, 5 :: Int)) :: ([Int], (Int, Int))
-- ([2,4,6],(8,10))
class ExtandableMap a b c d where
  extendMap :: (a -> b) -> c -> d

-- | Trivial instance of `ExtandableMap`.
instance ExtandableMap a b a b where
  extendMap = id

-- | Tuple instance of `ExtandableMap`.
instance
  (ExtandableMap a b c0 d0, ExtandableMap a b c1 d1) =>
  ExtandableMap a b (c0, c1) (d0, d1)
  where
  extendMap :: (a -> b) -> (c0, c1) -> (d0, d1)
  extendMap f = bimap (extendMap f) (extendMap f)

-- | Triple instance of `ExtandableMap`.
instance
  (ExtandableMap a b c0 d0, ExtandableMap a b c1 d1, ExtandableMap a b c2 d2) =>
  ExtandableMap a b (c0, c1, c2) (d0, d1, d2)
  where
  extendMap f (x0, x1, x2) = (extendMap f x0, extendMap f x1, extendMap f x2)

-- | List `[]` instance of `ExtandableMap`.
instance (ExtandableMap a b c d) => ExtandableMap a b [c] [d] where
  extendMap f = fmap (extendMap f)

-- | Sized vector instance of `ExtandableMap`.
instance
  (ExtandableMap a b c d) =>
  ExtandableMap a b (DVGS.Vector DV.Vector n c) (DVGS.Vector DV.Vector n d)
  where
  extendMap f = fmap (extendMap f)

-- | Stream instance of `ExtandableMap`.
instance (ExtandableMap a b c d) => ExtandableMap a b (Stream c) (Stream d) where
  extendMap f = fmap (extendMap f)

-- | Function `(->) r` instance of `ExtandableMap`.
instance
  (ExtandableMap a b c d) =>
  ExtandableMap a b (r -> c) (r -> d)
  where
  extendMap :: (a -> b) -> (r -> c) -> (r -> d)
  extendMap f = (.) (extendMap f)
