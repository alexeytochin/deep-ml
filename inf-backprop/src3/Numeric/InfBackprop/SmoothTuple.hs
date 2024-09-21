module Numeric.InfBackprop.SmoothTuple where

import Prelude (undefined, ($), id, (.))
import Numeric.InfBackprop.DLens (DLens(DLens), unpackDLens)
import NumHask ((+), Additive)
import GHC.Base (Type)


--class LensableD a cb b where
--  toLensD :: a -> DLens ct t cb b
--
--instance LensableD (DLens ct t cb b) cb b where
--  -- toLensD :: (DLens ct t cb0 b0, DLens ct t cb1 b1) -> DLens ct t (cb0, cb1) (b0, b1)
--  toLensD = id
--
--instance LensableD (DLens ct t cb0 b0, DLens ct t cb1 b1) (cb0, cb1) (b0, b1) where
--  toLensD :: (DLens ct t cb0 b0, DLens ct t cb1 b1) -> DLens ct t (cb0, cb1) (b0, b1)
--  toLensD = undefined


class LensDMap a b where
  to :: a -> b

instance LensDMap a a where
  to = id

--instance (Additive ct) => LensDMap (DLens ct t cb0 b0, DLens ct t cb1 b1) (DLens ct t (cb0, cb1) (b0, b1)) where
--  to (f, g) = DLens $ \t -> let
--     (y0, dxt0) = unpackDLens f t
--     (y1, dxt1) = unpackDLens g t
--   in ((y0, y1), \(dx0, dx1) -> dxt0 dx0 + dxt1 dx1)

instance (Additive ct, LensDMap (b0, b1) c, LensDMap cc (cb0, cb1)) => 
  LensDMap (DLens ct t cb0 b0, DLens ct t cb1 b1) (DLens ct t cc c) where
    to (f, g) = DLens $ \t -> let
       (y0, dxt0) = unpackDLens f t
       (y1, dxt1) = unpackDLens g t
     in (to (y0, y1), (\(dx0, dx1) -> dxt0 dx0 + dxt1 dx1) . to)


class Tuple b0 b1 c where
 toLensD :: (b0, b1) -> c

instance Tuple b0 b1 (b0, b1) where
  toLensD = id

instance (Additive ct) => Tuple (DLens ct t cb0 b0) (DLens ct t cb1 b1) (DLens ct t (cb0, cb1) (b0, b1)) where
  toLensD (f, g) = DLens $ \t -> let
      (y0, dxt0) = unpackDLens f t
      (y1, dxt1) = unpackDLens g t
    in (toLensD (y0, y1), \(dx0, dx1) -> dxt0 dx0 + dxt1 dx1)

instance Tuple (DLens ct t (DLens cu u ccb0 cb0) (DLens cu u cb0 b0)) (DLens ct t (DLens cu u ccb1 cb1) (DLens cu u cb1 b1)) (DLens ct t (DLens cu u (ccb0, ccb1) (cb0, cb1)) (DLens cu u (cb0, cb1) (b0, b1))) where
  toLensD = undefined

instance (Additive ct, Tuple b0 b1 c) => --  ,, Tuple cb0 cb1 cc) =>
  Tuple (DLens ct t cb0 b0) (DLens ct t cb1 b1) (DLens ct t cc c) where
    toLensD (f, g) = DLens $ \t -> let
        (y0, dxt0) = unpackDLens f t
        (y1, dxt1) = unpackDLens g t
      in (toLensD (y0, y1), \(dx0, dx1) -> dxt0 dx0 + dxt1 dx1)





class SmoothTuple b0 b1 c where
  smoothTuple :: forall a. (a -> b0) -> (a -> b1) -> a -> c

instance SmoothTuple b0 b1 (b0, b1) where
  smoothTuple :: forall a. (a -> b0) -> (a -> b1) -> a -> (b0, b1)
  smoothTuple f g x = (f x, g x)

instance (Additive ct) => 
  SmoothTuple (DLens ct t cb0 b0) (DLens ct t cb1 b1) (DLens ct t (cb0, cb1) (b0, b1)) where
    smoothTuple :: forall a. (a -> DLens ct t cb0 b0) -> (a -> DLens ct t cb1 b1) -> a -> DLens ct t (cb0, cb1) (b0, b1)
    smoothTuple f g x = DLens $ \t -> let
        (y0, dxt0) = unpackDLens (f x) t
        (y1, dxt1) = unpackDLens (g x) t
      in ((y0, y1), \(dx0, dx1) -> dxt0 dx0 + dxt1 dx1)


--smoothTuple0 :: (LensD ct t ca a -> b0) -> (LensD ct t ca a -> b1) -> LensD ct t ca a -> (b0, b1)
--smoothTuple0 = undefined