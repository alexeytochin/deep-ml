module Numeric.InfBackprop
  (
    Diff(MkDiff),
    derivative,
    CT,
    gradient,
    value,
    initDiff,
    tupleDerivative,
    derivative2ArgsOverY,
    twoArgsDerivative,
    stopDiff
  )
where

import Numeric.InfBackprop.Tangent (CT)
import Numeric.InfBackprop.Diff (Diff(MkDiff), derivative, gradient, value, initDiff, tupleDerivative, twoArgsDerivative,
    derivative2ArgsOverY, stopDiff
  )