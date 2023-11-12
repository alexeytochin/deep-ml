module Prelude.Tools where

cross :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
cross f g (x, y) = (f x, g y)

fork :: (t -> a) -> (t -> b) -> t -> (a, b)
fork f g x = (f x, g x)