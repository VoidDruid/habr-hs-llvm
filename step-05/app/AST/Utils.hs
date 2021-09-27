module AST.Utils where

import Data.List
import Data.Maybe

import StringUtils
import Syntax

extractFuncRet :: Expr -> Maybe Expr
extractFuncRet (Syntax.Function t n a r b) = case r of
  Nothing -> Nothing
  Just name -> Just $ Def t name


--- General purpose utilities

ppAST ast = putStrLn (joinedPrettyAST ast)
