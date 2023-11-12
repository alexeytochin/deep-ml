module Data.Profunctor.Extra (Outer, outer, (***)) where

import Data.Profunctor (Profunctor, dimap, Strong, first', second')
import Data.Bifunctor (bimap)
import Optics (Iso)

  
class Profunctor p =>
  Outer p q where
    outer :: forall a b c d. p a b -> p c d -> p (q a c) (q b d)

(***) :: forall p a b c d. Outer p (,) => p a b -> p c d -> p (a, c) (b, d)
(***) = outer
  
instance Outer (->) (,) where
  outer = bimap
