module Numeric.InfBackprop
  (
    BackpropDiff(MkBackpropDiff),
    derivative,
    simpleDerivative,
    derivativeOp,
    Cotangent,
    CT,
    gradient,
    value,
    backprop,
    initDiff,
    customArgDerivative,
    boxedVecArg,
    tupleDerivative,
    boxedVecDerivative,
    streamDerivative,
    derivative2ArgsOverY,
    twoArgsDerivative,
    stopDiff,
    BoxedVec,
    streamArg,
    DerivativeWith,
  )
where

import Numeric.InfBackprop.Tangent (CT, BoxedVec, Cotangent)
import Numeric.InfBackprop.BackpropDiff (BackpropDiff(MkBackpropDiff), derivative,
    gradient, value, initDiff, tupleDerivative, twoArgsDerivative,
    derivative2ArgsOverY, stopDiff, backprop, customArgDerivative, boxedVecArg, streamArg,
    simpleDerivative, derivativeOp, boxedVecDerivative, streamDerivative, DerivativeWith
  )