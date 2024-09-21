module Prelude.Tools (cross, cross3, fork, fork3, curry3, uncurry3, pureKleisli, assoc, disassoc) where

import Control.Arrow (Kleisli(Kleisli))
import Prelude (Monad, pure, (.), ($))


   
cross :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
cross f g (x, y) = (f x, g y)

cross3 :: (a0 -> b0) -> (a1 -> b1) -> (a2 -> b2) -> (a0, a1, a2) -> (b0, b1, b2)
cross3 f g h (x, y, z) = (f x, g y, h z)

{-# INLINE fork #-}
fork :: (t -> a) -> (t -> b) -> t -> (a, b)
fork f g x = (f x, g x)

{-# INLINE fork3 #-}
fork3 :: (t -> a0) -> (t -> a1) -> (t -> a2) -> t -> (a0, a1, a2)
fork3 f0 f1 f2 x = (f0 x, f1 x, f2 x)

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) = f x y z 

assoc :: ((a, b), c) -> (a, (b, c))
assoc ((a, b), c) = (a, (b, c))

disassoc :: (a, (b, c)) -> ((a, b), c)
disassoc (a, (b, c)) = ((a, b), c)

pureKleisli :: Monad m => (a -> b) -> Kleisli m a b
pureKleisli f = Kleisli $ pure . f



--class Mapper a b where
--  map :: (a -> a) -> (b -> b)