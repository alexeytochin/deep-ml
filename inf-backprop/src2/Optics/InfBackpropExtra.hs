module Optics.InfBackpropExtra where

import Optics (Iso', view, review, iso)
import Prelude.Tools (cross)
import Prelude (Functor, fmap)

--temp :: Iso s1 t1 a1 b1 -> s1 -> a1
--temp iso1 = view iso1

crossed :: Iso' s1 a1 -> Iso' s2 a2 -> Iso' (s1, s2) (a1, a2)
crossed iso1 iso2 = iso (cross (view iso1) (view iso2)) (cross (review iso1) (review iso2))

fmaped :: Functor f => 
  Iso' s a -> Iso' (f s) (f a)
fmaped i = iso (fmap (view i)) (fmap (review i))
