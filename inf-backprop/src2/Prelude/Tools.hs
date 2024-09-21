module Prelude.Tools (cross, fork, pureKleisli) where

import Control.Arrow (Kleisli(Kleisli))
import Prelude (Monad, pure, (.), ($))


   
cross :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
cross f g (x, y) = (f x, g y)

fork :: (t -> a) -> (t -> b) -> t -> (a, b)
fork f g x = (f x, g x)

pureKleisli :: Monad m => (a -> b) -> Kleisli m a b
pureKleisli f = Kleisli $ pure . f


--class Mapper a b where
--  map :: (a -> a) -> (b -> b)