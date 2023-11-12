{-# LANGUAGE UndecidableInstances #-} 
-- for DVF.Arity n

module Temp where

import Prelude (id, Functor, fmap, show, Int, String, Bool(True, False), (.))
import Prelude.Tools (cross)
import Data.Stream (Stream)
import qualified Data.Vector.Fixed.Boxed as DVFB
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Fixed as DVF
import qualified Data.Vector as DV
import GHC.TypeNats (KnownNat)

--import Data.Tuple.Ops (appPoly)
--import Prelude (Bool(False), show, String, Int)
--
--_ = appPoly show (5 :: Int, False) :: (String, String)

-- |
--
-- >>> import Prelude (show, Bool(False), (*))
-- 
-- >>> smartMap (show :: Int -> String) (42 :: Int) :: String
-- "42"
-- >>> smartMap (show :: Bool -> String) (True, False) :: (String, String)
-- ("True","False")
-- >>> smartMap ((*2) :: Int -> Int) (1 :: Int, (2 :: Int, 3 :: Int)) :: (Int, (Int, Int))
-- (2,(4,6))
-- >>> smartMap ((*2) :: Int -> Int) ([1, 2, 3] :: [Int], (4 :: Int, 5 :: Int)) :: ([Int], (Int, Int))
-- ([2,4,6],(8,10))
class Mapper a b c d where
  smartMap :: (a -> b) -> c -> d

instance Mapper a b a b where
  smartMap = id

instance (Mapper a b c0 d0, Mapper a b c1 d1) =>
  Mapper a b (c0, c1) (d0, d1) where
    smartMap :: (a -> b) -> (c0, c1) -> (d0, d1)
    smartMap f = cross (smartMap f) (smartMap f)

instance (Mapper a b c0 d0, Mapper a b c1 d1, Mapper a b c2 d2) =>
  Mapper a b (c0, c1, c2) (d0, d1, d2) where
    smartMap f (x0, x1, x2) = (smartMap f x0, smartMap f x1, smartMap f x2)

--instance (Functor f, Mapper a b c d) => Mapper a b (f c) (f d) where
--  smartMap f x = fmap (smartMap f) x

instance Mapper a b c d => Mapper a b [c] [d] where
  smartMap f x = fmap (smartMap f) x

instance (Mapper a b c d) => 
  Mapper a b (DVGS.Vector DV.Vector n c) (DVGS.Vector DV.Vector n d) where
    smartMap f x = fmap (smartMap f) x

instance (DVF.Arity n, Mapper a b c d) => 
  Mapper a b (DVFB.Vec n c) (DVFB.Vec n d) where
    smartMap f x = fmap (smartMap f) x

instance Mapper a b c d => Mapper a b (Stream c) (Stream d) where
  smartMap f x = fmap (smartMap f) x

instance (Mapper a b c d) =>
  Mapper a b (r -> c) (r -> d) where
    smartMap :: (a -> b) -> (r -> c) -> (r -> d)
    smartMap f = (.) (smartMap f)

-- _ = smartMap (show :: Bool -> String) (True, False) :: (String, String)