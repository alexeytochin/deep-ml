{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}

module Data.BoundedFix where

import Data.Fix (Fix(Fix, unFix))
import GHC.Natural (Natural)
import GHC.Show (Show)
import GHC.Generics (Generic)
import Prelude (max, undefined, ($), fmap, Functor, Foldable, (+), maximum, (-), (.), Monoid, Semigroup, (<>), mempty)
import Safe.Foldable (maximumDef)


--data FiniteList a = UnsafeMkFiniteList {length :: !Natural, getList :: [a]} deriving
--  (Show)

data B f a = UnsafeB {
  depth :: !Natural,
  unB :: f a
} deriving (Generic)

instance Functor f => Functor (B f) where
  fmap map (UnsafeB d fa) = UnsafeB d (fmap map fa)

type BFix f = Fix (B f)

mkBFix :: (Functor f, Foldable f) =>
  f (BFix f) -> BFix f
mkBFix fBFix = Fix $ UnsafeB newDepth fBFix where
  newDepth = maximumDef 0 (fmap (depth . unFix) fBFix) + 1

unBFix :: BFix f -> f (BFix f)
unBFix = unB . unFix 

depthBFix :: BFix f -> Natural
depthBFix = depth . unFix 

--data BFix f = UnsafeMkBFix {
--  depth :: !Natural,
--  unBFix :: f (BFix f)
--} deriving (Generic)
--
--mkBFix :: (Functor f, Foldable f) =>
--  f (BFix f) -> BFix f
--mkBFix fBFix = UnsafeMkBFix newDepth fBFix where
--  newDepth = maximumDef 0 (fmap depth fBFix) + 1


data ListF a b = NilF | ConsF a b deriving (Show, Functor, Foldable)

type BList a = BFix (ListF a)

emptyBList :: BList a
emptyBList = mkBFix NilF

consBList :: a -> BList a -> BList a
consBList x bls = mkBFix $ ConsF x bls

instance Semigroup (BList a) where
  x <> y = case unBFix x of
    NilF -> y
    ConsF a ls -> consBList a (ls <> y)
 
instance Monoid (BList a) where
  mempty = emptyBList