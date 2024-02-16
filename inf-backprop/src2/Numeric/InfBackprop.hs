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
    intPow
  )
where
  
import InfBackprop.Common6 (derivative)
import InfBackprop.LensD (intPow)
