{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module    :  InfBackprop
-- Copyright   :  (C) 2024 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Automatic differentiation and backpropagation.
-- See 'InfBackprop.Tutorial' for details.
module Numeric.InfBackprop
  (
    derivative,
    customDerivative,
    tupleLensIso,
    smallVecLensIso,
    idIso,
    intPow,
    LensD(LensD),
  )
where
  
import InfBackprop.Common6 (derivative, customDerivative)
import InfBackprop.LensD (intPow, tupleLensIso, idIso, LensD(LensD))
import InfBackprop.Vector (smallVecLensIso)
