-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE BangPatterns #-}

module Data.FiniteList (
    FiniteList,
    getList,
    length,
    unit,
    join,
    emptyFiniteList,
    consFiniteList,
    zipWithStream,
    zipWithStream2
  ) where

import Prelude (Functor, Monad, Applicative, Show, Monoid, Semigroup, (+), (-), fmap, (<>), mempty, fromIntegral,
  Foldable, (.), ($), flip)
import qualified Prelude as P
import Data.Vector.Fixed (VecList)
import GHC.Natural (Natural)
import NumHask (Distributive, one, zero, (*), Multiplicative, sum)
import Data.Stream (Stream, toList)


--newtype FiniteList a = UnsafeMkFiniteList [a] deriving
--  (Functor, Applicative, Monad, Show, Semigroup, Monoid)

--data FiniteList a = forall n. FiniteList {getVecList :: VecList n a}
--  
--FiniteListToList =  . getVecList

data FiniteList a = UnsafeMkFiniteList {length :: !Natural, getList :: [a]} deriving 
  (Show)

emptyFiniteList :: FiniteList a
emptyFiniteList = UnsafeMkFiniteList 0 []

join :: a -> FiniteList a -> FiniteList a
join x (UnsafeMkFiniteList n ls) = UnsafeMkFiniteList (n + 1) (x : ls)

consFiniteList :: a -> FiniteList a -> FiniteList a
consFiniteList x (UnsafeMkFiniteList l lx) = UnsafeMkFiniteList (l + 1) (x : lx) 

unit :: a -> FiniteList a
unit x = UnsafeMkFiniteList 1 [x]

replicate :: Natural -> a -> FiniteList a
replicate n x = UnsafeMkFiniteList n (P.replicate (fromIntegral n) x) 

basis :: Distributive a => Natural -> FiniteList a
basis n = case n of 
  0 -> emptyFiniteList
  1 -> unit one
  _ -> consFiniteList zero (basis (n - 1))
  
instance Functor FiniteList where
  fmap f (UnsafeMkFiniteList l lx) = UnsafeMkFiniteList l (fmap f lx) 

instance Semigroup (FiniteList a) where
  (UnsafeMkFiniteList l1 lx1) <> (UnsafeMkFiniteList l2 lx2) = UnsafeMkFiniteList (l1 + l2) (lx1 <> lx2) 
  
instance Monoid (FiniteList a) where
  mempty = emptyFiniteList

instance Foldable FiniteList where
  foldr :: (a -> b -> b) -> b -> FiniteList a -> b
  foldr f b (UnsafeMkFiniteList _ l) = P.foldr f b l

zipWithStream :: (a -> b -> c) -> Stream a -> FiniteList b -> FiniteList c
zipWithStream f s (UnsafeMkFiniteList n l) = UnsafeMkFiniteList n (P.zipWith f (toList s) l)

zipWithStream2 :: (a -> b -> c) -> FiniteList a -> Stream b -> FiniteList c
zipWithStream2 f = flip $ zipWithStream (flip f)


convWithStream :: Distributive a =>
  Stream a -> FiniteList a -> a
convWithStream a b = sum $ zipWithStream (*) a b
