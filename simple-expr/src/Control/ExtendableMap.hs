-- for DVF.Arity n

module Control.ExtendableMap where

import GHC.Base (id, fmap, (.)) --, Int, String, Bool(True, False), (.))
--import GHC.Show (show)
--import Prelude.Tools (cross)
import Data.Stream (Stream)
--import qualified Data.Vector.Fixed.Boxed as DVFB
import qualified Data.Vector.Generic.Sized as DVGS
--import qualified Data.Vector.Fixed as DVF
import qualified Data.Vector as DV
--import GHC.TypeNats (KnownNat)
import Data.Bifunctor (bimap)

--import Data.Tuple.Ops (appPoly)
--import Prelude (Bool(False), show, String, Int)
--
--_ = appPoly show (5 :: Int, False) :: (String, String)

-- |
--
-- >>> import GHC.Base (Bool(True, False), Int)
-- >>> import GHC.Show (show)
-- >>> import GHC.Num ((*))
-- >>> import Data.String (String)
-- 
-- >>> extendMap (show :: Int -> String) (42 :: Int) :: String
-- "42"
-- >>> extendMap (show :: Bool -> String) (True, False) :: (String, String)
-- ("True","False")
-- >>> extendMap ((*2) :: Int -> Int) (1 :: Int, (2 :: Int, 3 :: Int)) :: (Int, (Int, Int))
-- (2,(4,6))
-- >>> extendMap ((*2) :: Int -> Int) ([1, 2, 3] :: [Int], (4 :: Int, 5 :: Int)) :: ([Int], (Int, Int))
-- ([2,4,6],(8,10))
class ExtandableMap a b c d where
  extendMap :: (a -> b) -> c -> d

instance ExtandableMap a b a b where
  extendMap = id

instance (ExtandableMap a b c0 d0, ExtandableMap a b c1 d1) =>
  ExtandableMap a b (c0, c1) (d0, d1) where
    extendMap :: (a -> b) -> (c0, c1) -> (d0, d1)
    extendMap f = bimap (extendMap f) (extendMap f)

instance (ExtandableMap a b c0 d0, ExtandableMap a b c1 d1, ExtandableMap a b c2 d2) =>
  ExtandableMap a b (c0, c1, c2) (d0, d1, d2) where
    extendMap f (x0, x1, x2) = (extendMap f x0, extendMap f x1, extendMap f x2)

--instance (Functor f, Mapper a b c d) => Mapper a b (f c) (f d) where
--  smartMap f x = fmap (smartMap f) x

instance ExtandableMap a b c d => ExtandableMap a b [c] [d] where
  extendMap f x = fmap (extendMap f) x

instance (ExtandableMap a b c d) =>
  ExtandableMap a b (DVGS.Vector DV.Vector n c) (DVGS.Vector DV.Vector n d) where
    extendMap f x = fmap (extendMap f) x

--instance (DVF.Arity n, ExtandableMap a b c d) =>
--  ExtandableMap a b (DVFB.Vec n c) (DVFB.Vec n d) where
--    extendMap f x = fmap (extendMap f) x

instance ExtandableMap a b c d => ExtandableMap a b (Stream c) (Stream d) where
  extendMap f x = fmap (extendMap f) x

instance (ExtandableMap a b c d) =>
  ExtandableMap a b (r -> c) (r -> d) where
    extendMap :: (a -> b) -> (r -> c) -> (r -> d)
    extendMap f = (.) (extendMap f)


-- _ = smartMap (show :: Bool -> String) (True, False) :: (String, String)