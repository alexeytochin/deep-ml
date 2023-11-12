module NumHask.AdvancedActions where

import NumHask (Additive, (+), zero, Multiplicative, (*), one)
import Prelude (fmap, ($), Functor, undefined, const)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Fixed.Cont as DVFC
import qualified Data.Vector.Fixed.Boxed as DVFB
import qualified Data.Vector.Fixed as DVF
import qualified Data.Vector as DV
import GHC.TypeNats (KnownNat)



--instance (Additive a, Functor f) => Additive (f a) where
--  a + b = fmap (+)
--  zero = undefined 


class AdditiveAction a b where
  (.+) :: a -> b -> b
  zeroAction :: Proxy b -> a

instance Additive a => 
  AdditiveAction a a where
    (.+) = (+)
    zeroAction (Proxy :: Proxy a) = zero

instance AdditiveAction a b => 
  AdditiveAction a (r -> b) where
    fa .+ fb = \r -> fa .+ fb r 
    zeroAction (Proxy :: Proxy (r -> b)) = zeroAction (Proxy :: Proxy b)

-- zeroAction b0 = zeroAction b1
instance (AdditiveAction a b0, AdditiveAction a b1) => 
  AdditiveAction a (b0, b1) where
    fa .+ (fb0, fb1) = (fa .+ fb0, fa .+ fb1) 
    zeroAction (Proxy :: Proxy (b0, b1)) = zeroAction (Proxy :: Proxy b0)

instance AdditiveAction a b => 
  AdditiveAction a [b] where
    fa .+ fb = fmap (fa .+) fb 
    zeroAction (Proxy :: Proxy [b]) = zeroAction (Proxy :: Proxy b)

--instance (DVF.Arity n, AdditiveAction a b) => 
--  AdditiveAction a (DVFB.Vec n b) where
--    fa .+ fb = fmap (fa .+) fb 
--    zeroAction (Proxy :: Proxy [b]) = zeroAction (Proxy :: Proxy b)

instance (AdditiveAction a b) => 
  AdditiveAction a (DVGS.Vector DV.Vector n b) where
    fa .+ fb = fmap (fa .+) fb 
    zeroAction (Proxy :: Proxy (DVGS.Vector DV.Vector n b)) = zeroAction (Proxy :: Proxy b)


--instance AdditiveAction a b => 
--  AdditiveAction (r -> a) (r -> b) where
--    fa .+ fb = \r -> fa r .+ fb r 
--    zeroAction (Proxy :: Proxy (r -> b)) = const $ zeroAction (Proxy :: Proxy b)

  
--instance Additive a => AdditiveAction a [a] where
--  a .+ b = fmap (a+) b
  
--instance (Additive a, Functor f) => AdditiveAction a (f a) where
--  a .+ b = fmap (a+) b
  
instance (AdditiveAction a b, Functor f) => 
  AdditiveAction a (f b) where
    a .+ b = fmap (a .+) b
    zeroAction = zeroAction

-- Left
class LeftMultiplicativeAction a b where
  (.*) :: a -> b -> b
  leftIdentityAction :: Proxy b -> a

instance Multiplicative a => 
  LeftMultiplicativeAction a a where
    (.*) = (*)
    leftIdentityAction (Proxy :: Proxy a) = one

instance LeftMultiplicativeAction a b =>
  LeftMultiplicativeAction a [b] where
    fa .* fb = fmap (fa .*) fb 
    leftIdentityAction (Proxy :: Proxy [b]) = leftIdentityAction (Proxy :: Proxy b)

--instance (DVF.Arity n, MultiplicativeAction a b) => 
--  MultiplicativeAction a (DVFB.Vec n b) where
--    fa .* fb = fmap (fa .*) fb 
--    identityAction (Proxy :: Proxy [b]) = identityAction (Proxy :: Proxy b)

instance (LeftMultiplicativeAction a b) =>
  LeftMultiplicativeAction a (DVGS.Vector DV.Vector n b) where
    fa .* fb = fmap (fa .*) fb 
    leftIdentityAction (Proxy :: Proxy (DVGS.Vector DV.Vector n b)) = leftIdentityAction (Proxy :: Proxy b)


-- Right
class RightMultiplicativeAction a b where
  (*.) :: b -> a -> b
  rightIdentityAction :: Proxy b -> a

instance Multiplicative a =>
  RightMultiplicativeAction a a where
    (*.) = (*)
    rightIdentityAction (Proxy :: Proxy a) = one

instance RightMultiplicativeAction a b =>
  RightMultiplicativeAction a [b] where
    fa *. fb = fmap (*. fb) fa
    rightIdentityAction (Proxy :: Proxy [b]) = rightIdentityAction (Proxy :: Proxy b)

--instance (DVF.Arity n, RightMultiplicativeAction a b) =>
--  RightMultiplicativeAction a (DVFB.Vec n b) where
--    fa .* fb = fmap (fa .*) fb
--    identityAction (Proxy :: Proxy [b]) = identityAction (Proxy :: Proxy b)

instance (RightMultiplicativeAction a b) =>
  RightMultiplicativeAction a (DVGS.Vector DV.Vector n b) where
    fa *. fb = fmap (*. fb) fa
    rightIdentityAction (Proxy :: Proxy (DVGS.Vector DV.Vector n b)) = rightIdentityAction (Proxy :: Proxy b)



-- Symmetric
-- asociative, leftIdentityAction == rightIdentityAction
class (LeftMultiplicativeAction a b, RightMultiplicativeAction a b) =>
  LeftRightMultiplicativeAction a b where
    identityAction :: Proxy b -> a
    identityAction = leftIdentityAction

instance Multiplicative a =>
  LeftRightMultiplicativeAction a a where
    identityAction (Proxy :: Proxy a) = one

instance LeftRightMultiplicativeAction a b =>
  LeftRightMultiplicativeAction a [b] where
    identityAction (Proxy :: Proxy [b]) = identityAction (Proxy :: Proxy b)

--instance (DVF.Arity n, RightMultiplicativeAction a b) =>
--  RightMultiplicativeAction a (DVFB.Vec n b) where
--    identityAction (Proxy :: Proxy [b]) = identityAction (Proxy :: Proxy b)

instance (LeftRightMultiplicativeAction a b) =>
  LeftRightMultiplicativeAction a (DVGS.Vector DV.Vector n b) where
    identityAction (Proxy :: Proxy (DVGS.Vector DV.Vector n b)) = identityAction (Proxy :: Proxy b)