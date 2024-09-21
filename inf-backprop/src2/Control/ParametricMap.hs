{-# LANGUAGE DataKinds #-}

module Control.ParametricMap where

import Prelude ((.), id, undefined)
import Data.Vector.Fixed.Cont (PeanoNum(Z, S))
import GHC.Types (Type)


pMap0 :: (a -> b) -> a -> b
pMap0 = id

pMap1 :: (a -> b) -> (r -> a) -> (r -> b)
pMap1 = (.)

pMap2 :: (a -> b) -> (r0 -> r1 -> a) -> (r0 -> r1 -> b)
pMap2 = (.) . (.)

pMap3 :: (a -> b) -> (r0 -> r1 -> r2 -> a) -> (r0 -> r1 -> r2 -> b)
pMap3 = (.) . (.) . (.)

pMapStep :: ((a -> b) -> a1 -> b1) -> ((a -> b) -> (r -> a1) -> r -> b1)
pMapStep f = (.) . f

-- pMapN (Proxy (n :: PeanoNum)) =

data PeanoNum2 a = Z2 | S2 PeanoNum a

type family Fn (n :: PeanoNum) (a :: Type) (b :: Type)
type instance Fn 'Z a b = b
type instance Fn ('S n) a b = a -> Fn n a b

type family AddParam (a :: Type) (b :: Type)
type instance AddParam a b = a -> b

type family Temp (n :: PeanoNum) (a :: Type) (b :: Type) (r :: Type)
type instance Temp 'Z a b r = a -> b
type instance Temp ('S n) a b r = (a -> b) -> a -> b

--pMapN :: (a -> b) -> c ->




--data AddParam  = NoParam | Add a AddParam


--type AddParam a b = a -> b

--type family AddParam (a :: Type) (b :: Type)
--type instance AddParam a b = a -> b
--type instance Fn 'Z a b = b
--type instance Fn ('S n) a b = a -> Fn n a b


-- opPowN :: Proxy  (a -> b) ->