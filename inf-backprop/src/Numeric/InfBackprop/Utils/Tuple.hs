-- | Module    :  Numeric.InfBackprop.Instances.NumHask
-- Copyright   :  (C) 2025 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Utility functions for working with tuples.
module Numeric.InfBackprop.Utils.Tuple
  ( cross,
    cross3,
    fork,
    fork3,
    curry3,
    uncurry3,
    biCross,
    biCross3,
  )
where

-- | Applies two functions to the components of a tuple.

--- ==== __Examples__
--
-- >>> cross (+1) (*2) (3, 4)
-- (4,8)
cross :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
{-# INLINE cross #-}
cross f g (x, y) = (f x, g y)

-- | Applies three functions to the components of a triple.
--
-- ==== __Examples__
--
-- >>> import GHC.Num ((+), (-), (*))
--
-- >>> cross3 (+1) (*2) (\x -> x - 3) (3, 4, 10)
-- (4,8,7)
cross3 :: (a0 -> b0) -> (a1 -> b1) -> (a2 -> b2) -> (a0, a1, a2) -> (b0, b1, b2)
{-# INLINE cross3 #-}
cross3 f g h (x, y, z) = (f x, g y, h z)

-- | Applies two functions to the same argument and returns a tuple of results.
--
-- ==== __Examples__
--
-- >>> import GHC.Num ((+), (*))
--
-- >>> fork (+1) (*2) 3
-- (4,6)
fork :: (t -> a) -> (t -> b) -> t -> (a, b)
{-# INLINE fork #-}
fork f g x = (f x, g x)

-- | Applies three functions to the same argument and returns a triple of results.
--
-- >>> import GHC.Num ((+), (-), (*))
--
-- ==== __Examples__
--
-- >>> fork3 (+1) (*2) (\x -> x - 3) 5
-- (6,10,2)
fork3 :: (t -> a0) -> (t -> a1) -> (t -> a2) -> t -> (a0, a1, a2)
{-# INLINE fork3 #-}
fork3 f0 f1 f2 x = (f0 x, f1 x, f2 x)

-- | Curries a function on triples.
--
-- ==== __Examples__
--
-- >>> import GHC.Num ((+))
--
-- >>> f (x, y, z) = x + y + z
-- >>> g = curry3 f
-- >>> g 1 2 3
-- 6
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
{-# INLINE curry3 #-}
curry3 f x y z = f (x, y, z)

-- | Uncurries a function on triples.
--
-- ==== __Examples__
--
-- >>> import GHC.Num ((+))
--
-- >>> f x y z = x + y + z
-- >>> g = uncurry3 f
-- >>> g (1, 2, 3)
-- 6
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
{-# INLINE uncurry3 #-}
uncurry3 f (x, y, z) = f x y z

-- | Applies two binary functions to the components of two tuples.
--
-- ==== __Examples__
--
-- >>> import GHC.Num ((+), (*))
--
-- >>> biCross (+) (*) (1, 2) (3, 4)
-- (4,8)
biCross :: (a -> b -> c) -> (d -> e -> f) -> (a, d) -> (b, e) -> (c, f)
{-# INLINE biCross #-}
biCross f g (x0, x1) (y0, y1) = (f x0 y0, g x1 y1)

-- | Applies three binary functions to the components of two triples.
--
-- ==== __Examples__
--
-- >>> import GHC.Num ((+), (*), (-))
--
-- >>> biCross3 (+) (*) (-) (1, 2, 10) (3, 4, 5)
-- (4,8,5)
biCross3 ::
  (a -> b -> c) ->
  (d -> e -> f) ->
  (g -> h -> l) ->
  (a, d, g) ->
  (b, e, h) ->
  (c, f, l)
{-# INLINE biCross3 #-}
biCross3 f g h (x0, x1, x2) (y0, y1, y2) = (f x0 y0, g x1 y1, h x2 y2)
