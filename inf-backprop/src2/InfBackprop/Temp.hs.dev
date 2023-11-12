{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module InfBackprop.Temp where

import NumHask (Additive, (+), zero)
import Prelude ((.), id, fst, snd, uncurry, curry, ($), undefined, const)


data D dx x dy y  = D {
  view    :: x -> y,
  update  :: x -> dy -> dx
}

instance (Additive y, Additive dx) => 
  Additive (D dx x dy y) where
    zero = D (const zero) (const zero)
    (D v1 u1) + (D v2 u2) = D (\x -> v1 x + v2 x) (\dy x -> u1 dy x + u2 dy x)


class Sum a b | a -> b where -- | a -> b  
  add :: a -> b

instance Additive a => 
  Sum (a, a) a where
    add :: (a, a) -> a
    add = uncurry (+)

instance (Sum a b, Split db da) => 
  Sum (D dt t da a) (D dt t db b) where
    add :: D dt t da a -> D dt t db b
    add (D v u) = D (add . v) (\t db -> u t (split db))


class Split a b | b -> a where -- | a -> b
  split :: a -> b

instance Split a (a, a) where
  split :: a -> (a, a)
  split = \x -> (x, x)

instance (Split a b, Sum db da) => 
  Split (D dt t da a) (D dt t db b) where
    split :: D dt t da a -> D dt t db b
    split (D v u) = D (split . v) (\t db -> u t (add db))