-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE BangPatterns #-}

module Data.FiniteList -- (
--    BoundedStream,
--    getList,
--    length,
--    unit,
--    bJoin,
--    bHead,
--    bTail,
--    emptyFiniteList,
--    replicate,
--    basis,
--    -- consFiniteList,
--    convWithStream,
--    zipWithStream,
--    zipWithStream2
--  ) 
where

import Prelude (Functor, Monad, Applicative, Show, Monoid, Semigroup, fmap, (<>), mempty, fromIntegral,
  Foldable, (.), ($), flip, Maybe(Just, Nothing), max, length)
import qualified Prelude as P
import Data.Vector.Fixed (VecList)
import GHC.Natural (Natural, minusNatural)
import NumHask (Distributive, one, zero, (*), Multiplicative, sum, Additive, (+), (-), (*))
import Prelude hiding (map, iterate, repeat, unzip, sum, head, tai, (-), (+), (*))
import Data.Stream (Stream(Cons), (<:>), repeat, iterate, map, head, tail, fromList, toList)
-- import Debug.Trace (trace)
import Control.ExtendableMap (ExtandableMap, extendMap)

--newtype FiniteList a = UnsafeMkFiniteList [a] deriving
--  (Functor, Applicative, Monad, Show, Semigroup, Monoid)

--data FiniteList a = forall n. FiniteList {getVecList :: VecList n a}
--  
--FiniteListToList =  . getVecList


data BoundedStream a = UnsafeMkFiniteList {length :: Natural, getList :: [a]}
-- data BoundedStream a = UnsafeMkFiniteList {length :: !Natural, getList :: [a]} deriving



bHead :: Additive a => BoundedStream a -> a
bHead (UnsafeMkFiniteList _ []) = zero
bHead (UnsafeMkFiniteList _ (x : _)) = x

bTail :: BoundedStream a -> BoundedStream a
bTail (UnsafeMkFiniteList _ []) = emptyFiniteList
bTail (UnsafeMkFiniteList n (_ : ls)) = if n > 0
--  then trace ("Making a BoundedStream of lenght " <> show (n - 1) <> " with list of length " <> show (Prelude.length ls)) (UnsafeMkFiniteList (n - 1) ls)
--  then trace ("Making a BoundedStream of lenght " <> show (minusNatural n 1)) (UnsafeMkFiniteList (minusNatural n 1) ls)
  then UnsafeMkFiniteList (minusNatural n 1) ls
  else error "bTail: negative length"

bDualNext :: (BoundedStream a -> b) -> (a -> b, BoundedStream a -> b)
bDualNext f = (f . unit, f . bTail)


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

basisStream :: a -> a -> Stream (BoundedStream a)
--basisStream zero' one' = iterate (bJoin zero') (unit one')
basisStream zero' one' = map (bJoin one') (zeroBasisStream zero')

zeroBasisStream :: a -> Stream (BoundedStream a)
zeroBasisStream zero' = iterate (bJoin zero') emptyFiniteList

instance Show a => Show (BoundedStream a) where
  show (UnsafeMkFiniteList n l) = "Bounded stream of length = " <> show n <> ", value = " <> mconcat (fmap (\x -> show x <> ", ") l) <> "0, 0, 0, ..." 

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

instance ExtandableMap a b c d => ExtandableMap a b (BoundedStream c) (BoundedStream d) where
  extendMap f x = fmap (extendMap f) x

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
