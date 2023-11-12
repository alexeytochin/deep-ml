{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module    :  InfBackprop
-- Copyright   :  (C) 2024 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Automatic differentiation and backpropagation.
-- See 'Numeric.InfBackprop.Tutorial' for details.

module Numeric.InfBackprop
  (
    derivative
  )
where
  
import Numeric.InfBackprop.DIfferentiable3 (derivative)
