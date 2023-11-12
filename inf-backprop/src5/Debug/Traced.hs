{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-export-lists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Debug.Traced where

import NumHask (Additive, (+) , Distributive, Divisive, ExpField, Multiplicative, Subtractive, TrigField, fromInteger, zero, 
  (*), MultiplicativeAction, (^^), Integral)
import GHC.Show (Show)
import GHC.Base (Eq)
import Data.Hashable (Hashable)

newtype Traced a = MkTraced a
  deriving (Show, Eq, Additive, Multiplicative, Divisive, ExpField, TrigField, Subtractive, Hashable)

#if MIN_VERSION_numhask(0,11,0)
#else
instance Distributive Traced
#endif

