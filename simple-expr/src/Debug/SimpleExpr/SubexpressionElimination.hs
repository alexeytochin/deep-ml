module Debug.SimpleExpr.SubexpressionElimination where

import Debug.SimpleExpr (SimpleExpr)
import Data.Fix (Fix (Fix, unFix))


copySimpleExprF :: SimpleExprF a -> State SimpleExprF a
copySimpleExprF e = case e of
    n@(Fix (NumberF _)) -> n
    c@(Fix (VariableF _)) -> c
    s@(Fix (SymbolicFuncF _ l)) -> s



copySimpleExpr :: SimpleExpr -> SimpleExpr
copySimpleExpr e = case e of
    n@(Fix (NumberF _)) -> n
    c@(Fix (VariableF _)) -> c
    s@(Fix (SymbolicFuncF _ l)) -> s
