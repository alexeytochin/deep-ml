{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module    :  Debug.SimpleExpr.Tutorial
-- Copyright   :  (C) 2023 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Tutorial / Quick strat / Demo for 'simple-expr' package.
--
-- [test](#simple_expr_tutorial_head)
module Debug.SimpleExpr.Tutorial
  ( -- * Quick start
    -- $quick_start2

    -- * Expression simplification
    -- $expression_simplification

    -- * Visualisation
    -- $visualisation
  )
where

-- import Debug.SimpleExpr
-- import Debug.SimpleExpr.GraphUtils

import Control.Concurrent (ThreadId)
import Data.Graph.DGraph (DGraph)
import Debug.SimpleExpr
import Debug.SimpleExpr.GraphUtils
import NumHask (sin, (**))
import Prelude (FilePath, IO, String)

-- $quick_start2 #simple_expr_tutorial_head#
-- >>> import Prelude (String)
-- >>> import Debug.SimpleExpr (variable, unaryFunc, binaryFunc)
-- >>> import NumHask (sin, (**))
--
-- Let us build an expression for
--
-- \[
--   f(x) := \sin x^2
-- \]
--
-- It can be done as follows
--
-- >>> x = variable "x"
-- >>> sin (x ** 2)
-- sin(x^2)
--
-- where @x@ and @sin (x ** 2)@ have type 'SimpleExpr',
-- 'variable'@ :: @'String'@ -> @'SimpleExpr'
-- and
-- 'sin'@ :: @'SimpleExpr'@ -> @'SimpleExpr'.
-- The type 'SimpleExpr' is just a syntactic tree where the role of leaves is played by
-- variables and numbers.
--
-- We can define a custom function using 'unaryFunc' and binary functoins using 'binaryFunc' as follows
--
-- >>> f = unaryFunc "f"
-- >>> (-*-) = binaryFunc "-*-"
-- >>> f x -*- f x
-- f(x)-*-f(x)

-- $expression_simplification
-- >>> import Prelude (($))
-- >>> import Debug.SimpleExpr (variable, simplify)
-- >>> import NumHask ((+), (-), (*))
--
-- We can try to simplify expressions with the aid of quite a primitive 'simplify' method
--
-- >>> x = variable "x"
-- >>> simplify $ (x + 0) * 1 - x * (3 - 2)
-- 0

-- $visualisation
-- >>> import Debug.SimpleExpr (variable, unaryFunc)
-- >>> import Debug.SimpleExpr.GraphUtils (plotExpr, plotDGraphPng, exprToGraph)
-- >>> import NumHask (exp, (*), (+), (-))
--
-- There is a built-in tool to visualize expression that attracts
-- [graphite](https://hackage.haskell.org/package/graphite)
-- package to transform expressions to
-- [graphs](https://hackage.haskell.org/package/graphs)
-- and
-- [graphviz](https://hackage.haskell.org/package/graphviz)
-- to render images.
--
-- Consider first a simple composition
--
-- >>> x = variable "x"
-- >>> f = unaryFunc "f"
-- >>> g = unaryFunc "g"
-- >>> expr = g (f x)
-- >>> expr
-- g(f(x))
--
-- It can be plotted by
-- 'plotExpr'@ :: @'Expr'@ d => d -> @'IO' 'ThreadId'
-- like
--
-- @ plotExpr expr @
--
-- ![image description](/home/alexey/Dropbox/projects/deep-ml/simple-expr/doc/images/composition.png)
--
-- To save the image use, for example,
--
-- 'plotDGraphPng'@ (@'exprToGraph'@ expr) pathToFile @,
--
-- where
--
-- 'exprToGraph'@ :: @'Expr'@ d => d -> @'DGraph' 'String'@ () @
--
-- and
--
-- 'plotDGraphPng'@ :: @'DGraph'@ v e -> @'FilePath'@ -> @'IO' 'FilePath'.
--
-- Consider now a more representative example
--
-- \[
--   e^{i k x} + e^{ - i k x}
-- \]
--
-- >>> :{
--   x, k, i, expr :: SimpleExpr
--   x = variable "x"
--   k = variable "k"
--   i = variable "i"
--   expr = exp (i * k * x) + exp (-(i * k * x))
-- :}
--
-- ![image description](doc/images/imaginary_expr_sum.png)
