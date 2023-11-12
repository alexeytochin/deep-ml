module InfBackprop.Stream where

import GHC.Natural (Natural)
import Data.Basis3 (Basis, End, initBackProp, zeroBackProp)
import Data.FiniteList (BoundedStream, emptyFiniteList, unit, bJoin, zipWithStream, zipWithStream2, bHead, bTail)
import Data.Stream (Stream(Cons), (<:>), repeat, iterate, map, head, tail, fromList)
import NumHask (Additive, sum, one, zero)
import InfBackprop.LensD (LensD(LensD))
import Prelude (($), (.), zip)
import Prelude hiding (map, iterate, repeat, unzip, sum, head, tail)
import Data.List.NonEmpty (unzip)
import Optics (Iso', iso)

streamBasis :: a -> a -> Stream (Stream a)
streamBasis zero' one' = iterate (zero' <:>) (one' <:> repeat zero')

enumerateList :: [a] -> [(Natural, a)]
enumerateList = zip [(0 :: Natural) ..]

instance Basis a =>
  Basis (Stream a) where
    type End (Stream a) b = Stream (End a b)

    initBackProp :: forall b. (Stream a -> b) -> Stream (End a b)
    initBackProp bp = map (\mkStream -> initBackProp (bp . mkStream)) mkStreams where
      mkStreams = iterate (\s a -> zeroBackProp <:> s a) (\a -> a <:> repeat zeroBackProp) :: Stream (a -> Stream a)

    zeroBackProp :: Stream a
    zeroBackProp = repeat zeroBackProp

instance Basis a =>
  Basis (BoundedStream a) where
    type End (BoundedStream a) b = Stream (End a b)

    initBackProp :: forall b. (BoundedStream a -> b) -> Stream (End a b)
    initBackProp bp = map (\mkStream -> initBackProp (bp . mkStream)) mkFiniteLists where
      mkFiniteLists = iterate (\s a -> bJoin zeroBackProp (s a)) unit :: Stream (a -> BoundedStream a)

    zeroBackProp :: BoundedStream a
    zeroBackProp = emptyFiniteList

streamToLens :: forall dt t dx x. (Additive dt) =>
 Stream (LensD dt t dx x) -> LensD dt t (BoundedStream dx) (Stream x)
streamToLens streamOfLens = LensD $ \t -> let
    (stream, dStreamT) = unzip (fmap (\(LensD f) -> f t) streamOfLens) :: (Stream x, Stream (dx -> dt))
  in (stream :: Stream x, sum . zipWithStream id dStreamT :: BoundedStream dx -> dt)

lensToStream :: forall dt t dx x. (Additive dt, Additive dx) =>
  LensD dt t (BoundedStream dx) (Stream x) -> Stream (LensD dt t dx x)
lensToStream (LensD lens) = Cons firstLens otherLenses where
  firstLens = LensD $ \t -> let
      (stream :: Stream x, dStream :: BoundedStream dx -> dt) = lens t
    in (head stream, dStream . unit)
  otherLenses = lensToStream $ LensD $ \t -> let 
      (stream :: Stream x, dStream :: BoundedStream dx -> dt) = lens t
    in (tail stream, dStream . bJoin zero)

streamLensIso :: (Additive dt, Additive dx) =>
  Iso' (LensD dt t (BoundedStream dx) (Stream x)) (Stream (LensD dt t dx x))
streamLensIso = iso lensToStream streamToLens

--longVecLensIso :: (KnownNat n, Additive dx, Additive dt) =>
--  Iso' (LensD dt t (DVGS.Vector DV.Vector n dx) (DVGS.Vector DV.Vector n x)) (DVGS.Vector DV.Vector n (LensD dt t dx x))
--longVecLensIso = iso lensTolongVec longVecToLens


boundedStreamToLens :: forall dt t dx x. (Additive dt) =>
  BoundedStream (LensD dt t dx x) -> LensD dt t (Stream dx) (BoundedStream x)
boundedStreamToLens finiteListOfLens = LensD $ \t -> let
    (finiteList, dFiniteListT) = unzip (fmap (\(LensD f) -> f t) finiteListOfLens) :: (BoundedStream x, BoundedStream (dx -> dt))
  in (finiteList :: BoundedStream x, sum . zipWithStream2 id dFiniteListT :: Stream dx -> dt)

lensToBoundedStream :: forall dt t dx x. (Additive dt, Additive x, Additive dx) =>
  LensD dt t (Stream dx) (BoundedStream x) -> BoundedStream (LensD dt t dx x)
lensToBoundedStream (LensD lens) = bJoin firstLens otherLenses where
  firstLens = LensD $ \t -> let
      (finiteList :: BoundedStream x, dFiniteList :: Stream dx -> dt) = lens t
    in (bHead finiteList, dFiniteList . (\x -> Cons x (repeat zero)))
  otherLenses = lensToBoundedStream $ LensD $ \t -> let
      (stream :: BoundedStream x, dStream :: Stream dx -> dt) = lens t
    in (bTail stream, dStream . Cons zero)

boundedStreamLensIso :: (Additive dt, Additive x, Additive dx) =>
  Iso' (LensD dt t (Stream dx) (BoundedStream x)) (BoundedStream (LensD dt t dx x))
boundedStreamLensIso = iso lensToBoundedStream boundedStreamToLens