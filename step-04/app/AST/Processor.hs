{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module AST.Processor where

import Control.Exception as E

import AST.Typing
import AST.Utils
import AST.Errors
import StringUtils
import Syntax

use [] x = x
use [f] x = f x
use (f:fs) x = use fs (f x)

-- TODO: better errors (!!!)
-- TODO: optimizations?
processAST :: AST -> Either TAST [GTypeError]
processAST ast = annotateTypes $ use -- TODO: change all funcs to Either TAST [e] and move annotateTypes in use list
  [ desugarFunctions
  ] ast

walkAST :: (Expr -> Expr) -> AST -> AST
process :: (Expr -> Expr) -> Expr  -> Expr

walkAST m = map (process m)
process m expr = m (process' m expr)

mapP m = map (process m)  -- same as walkAST, but is used on "lists" of exprs like args, insted of AST - in case processing changes

process' m (Block codeBlock)= Block (walkAST m codeBlock)
process' m (Call name args) = Call name (mapP m args)
process' m (Function t n args r body) = Function t n (mapP m args) r (walkAST m body)
process' m (BinaryOp op e1 e2) = BinaryOp op (process m e1) (process m e2)
process' m (UnaryOp op e) = UnaryOp op (process m e)
process' m (If cond brT brF) = If (process m cond) (walkAST m brT) (walkAST m brF)
process' m e = e

desugarFunctions = walkAST (\expr -> case expr of
    Function {} -> desugarFunc expr -- if I put desugarFunc logic here, GHC (!!!) crashes: <ghc: panic! (the 'impossible' happened)> 
    _ -> expr
    )

desugarFunc func@(Function t n a r body) =
  Function t n a Nothing $ case extractFuncRet func of
    Nothing -> body
    Just (Def t name) -> Def t name : body ++ [Var name]
