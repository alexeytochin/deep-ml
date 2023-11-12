{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module    :  Debug.SimpleExpr
-- Copyright   :  (C) 2023-2025 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Minimalistic toolkit for simple mathematical expression developed for debug purposes.
-- See 'Debug.SimpleExpr.Tutorial' for a quick introduction.
module Debug.SimpleExpr
  ( -- * Expression manipulation
    number,
    variable,
    unaryFunc,
    binaryFunc,
    simplifyExpr,
    simplify,

    -- * Base types
    SimpleExpr,
    SE,
    SimpleExprF,
    Expr,

    -- * Visualisation
    plotExpr,
    exprToGraph,
    plotDGraph,
    plotDGraphPng,

    -- * Auxiliary functions
    dependencies,
    content,
  )
where

import Data.Graph.VisualizeAlternative (plotDGraph)
import Debug.SimpleExpr.Expr
  ( Expr,
    SE,
    SimpleExpr,
    SimpleExprF,
    binaryFunc,
    content,
    dependencies,
    number,
    simplify,
    simplifyExpr,
    unaryFunc,
    variable,
  )
import Debug.SimpleExpr.GraphUtils (exprToGraph, plotDGraphPng, plotExpr)
