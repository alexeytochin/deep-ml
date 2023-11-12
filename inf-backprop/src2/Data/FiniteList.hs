-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE BangPatterns #-}

module Data.FiniteList (
    BoundedStream,
    getList,
    length,
    unit,
    bJoin,
    bHead,
    bTail,
    emptyFiniteList,
    replicate,
    basis,
    -- consFiniteList,
    convWithStream,
    zipWithStream,
    zipWithStream2
  ) where

import Prelude (Functor, Monad, Applicative, Show, Monoid, Semigroup, fmap, (<>), mempty, fromIntegral,
  Foldable, (.), ($), flip, Maybe(Just, Nothing), max)
import qualified Prelude as P
import Data.Vector.Fixed (VecList)
import GHC.Natural (Natural)
import NumHask (Distributive, one, zero, (*), Multiplicative, sum, Additive, (+), (-))
import Data.Stream (Stream, toList)


--newtype FiniteList a = UnsafeMkFiniteList [a] deriving
--  (Functor, Applicative, Monad, Show, Semigroup, Monoid)

--data FiniteList a = forall n. FiniteList {getVecList :: VecList n a}
--  
--FiniteListToList =  . getVecList

data BoundedStream a = UnsafeMkFiniteList {length :: !Natural, getList :: [a]} deriving
  (Show)

bHead :: Additive a => BoundedStream a -> a
bHead (UnsafeMkFiniteList _ []) = zero
bHead (UnsafeMkFiniteList _ (x : _)) = x

bTail :: BoundedStream a -> BoundedStream a
bTail (UnsafeMkFiniteList _ []) = emptyFiniteList
bTail (UnsafeMkFiniteList n (_: ls)) = UnsafeMkFiniteList (n - 1) ls



emptyFiniteList :: BoundedStream a
emptyFiniteList = UnsafeMkFiniteList 0 []

bJoin :: a -> BoundedStream a -> BoundedStream a
bJoin x (UnsafeMkFiniteList n ls) = UnsafeMkFiniteList (n + 1) (x : ls)

--bConcat :: BoundedStream a -> BoundedStream a -> BoundedStream a
--bConcat (UnsafeMkFiniteList length1 list1) (UnsafeMkFiniteList length2 list2) = 


--consFiniteList :: a -> BoundedStream a -> BoundedStream a
--consFiniteList x (UnsafeMkFiniteList l lx) = UnsafeMkFiniteList (l + 1) (x : lx)

unit :: a -> BoundedStream a
unit x = UnsafeMkFiniteList 1 [x]

replicate :: Natural -> a -> BoundedStream a
replicate n x = UnsafeMkFiniteList n (P.replicate (fromIntegral n) x) 

basis :: Distributive a => Natural -> BoundedStream a
basis n = case n of 
  0 -> emptyFiniteList
  1 -> unit one
  _ -> bJoin zero (basis (n - 1))
  
instance Functor BoundedStream where
  fmap f (UnsafeMkFiniteList l lx) = UnsafeMkFiniteList l (fmap f lx) 

instance Semigroup (BoundedStream a) where
  (UnsafeMkFiniteList l1 lx1) <> (UnsafeMkFiniteList l2 lx2) = UnsafeMkFiniteList (l1 + l2) (lx1 <> lx2) 
  
instance Monoid (BoundedStream a) where
  mempty = emptyFiniteList

instance Foldable BoundedStream where
  foldr :: (a -> b -> b) -> b -> BoundedStream a -> b
  foldr f b (UnsafeMkFiniteList _ l) = P.foldr f b l

instance (Additive a) => Additive (BoundedStream a) where
  zero = emptyFiniteList
  (UnsafeMkFiniteList nx lx) + (UnsafeMkFiniteList ny ly) = UnsafeMkFiniteList (max nx ny) (listSum lx ly) 

listSum :: Additive a => [a] -> [a] -> [a]
listSum [] y = y
listSum x [] = x
listSum (x : xs) (y : ys) = (x + y) : listSum xs ys
  
zipWithStream :: (a -> b -> c) -> Stream a -> BoundedStream b -> BoundedStream c
zipWithStream f s (UnsafeMkFiniteList n l) = UnsafeMkFiniteList n (P.zipWith f (toList s) l)

zipWithStream2 :: (a -> b -> c) -> BoundedStream a -> Stream b -> BoundedStream c
zipWithStream2 f = flip $ zipWithStream (flip f)


convWithStream :: Distributive a =>
  Stream a -> BoundedStream a -> a
convWithStream a b = sum $ zipWithStream (*) a b
