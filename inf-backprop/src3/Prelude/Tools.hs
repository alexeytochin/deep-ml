module Prelude.Tools (cross, cross3, fork, pureKleisli) where

import Control.Arrow (Kleisli(Kleisli))
import Prelude (Monad, pure, (.), ($))


   
cross :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
cross f g (x, y) = (f x, g y)

cross3 :: (a0 -> b0) -> (a1 -> b1) -> (a2 -> b2) -> (a0, a1, a2) -> (b0, b1, b2)
cross3 f g h (x, y, z) = (f x, g y, h z)

fork :: (t -> a) -> (t -> b) -> t -> (a, b)
fork f g x = (f x, g x)

pureKleisli :: Monad m => (a -> b) -> Kleisli m a b
pureKleisli f = Kleisli $ pure . f


--class Mapper a b where
--  map :: (a -> a) -> (b -> b)