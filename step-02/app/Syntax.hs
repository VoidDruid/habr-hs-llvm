{-# LANGUAGE TypeSynonymInstances #-}

module Syntax where

import Data.Maybe
import Data.Char (isSpace)

import StringUtils

class Show e => Pretty e where  -- used below for Expr and CodeBlock
  prettify :: e -> [String]

type Name = String
type CodeBlock term = [term]

data ExprType
  = VoidType
  | IntType
  | FloatType
  | BytesType
  | BooleanType
  | CallableType [ExprType] ExprType
  | AutoType
  deriving (Eq, Ord)

instance Show ExprType where
  show VoidType = "void"
  show IntType = "int"
  show FloatType = "float"
  show BytesType = "bytes"
  show BooleanType = "bool"
  show (CallableType from to) = "(" ++ fromRepr ++ " -> " ++ show to ++ ")"
    where
      fromRepr = case from of
        [] -> "()"
        from' -> joinC (map show from')
  show AutoType = "auto"

data Expr
  = Int Integer
  | Float Double
  | Var Name
  | Def ExprType Name
  | Block (CodeBlock Expr)
  | Call String [Expr]
  | Function ExprType Name [Expr] (Maybe Name) (CodeBlock Expr)
  | BinaryOp String Expr Expr
  | If Expr (CodeBlock Expr) (CodeBlock Expr)
  | While Expr (CodeBlock Expr)
  | TypeCast ExprType Expr
  deriving (Eq, Ord, Show)

type AST = [Expr]

--- Utilities

joinOrSplit :: Pretty a => [String] -> a -> [String] 
joinOrSplit s e = case prettify e of
  [r] ->  addToLast s (" (" ++ r ++ ")")
  listR -> addToLast s " (" ++ ["  " ++ r' | r' <- listR] ++ [")"]

prettifyAST :: Pretty e => [e] -> [String]
prettifyAST = map (joinN . prettify)

joinedPrettyAST :: Pretty e => [e] -> String
joinedPrettyAST = joinN . prettifyAST

instance Pretty e => Pretty (CodeBlock e) where
  prettify exprs = concatMap tabExpr exprs
    where tabExpr e = map ("  " ++) (prettify e)

smartJoin :: [String] -> [String]
smartJoin strs = if sum (map length strs) < 40
  then [joinS (map (dropWhile isSpace) strs)]
  else strs

instance Pretty Expr where
  prettify expr = case expr of
    (TypeCast type_ e) -> joinOrSplit ["(" ++ show type_ ++ ")"] e
    (Int i) -> [joinS ["Int", show i]]
    (Float f) -> [joinS ["Float", show f]]
    (Var n) -> [joinS ["Var", show n]]
    (Def t n) -> [joinS ["Def", show t, show n]]
    (Block es) -> smartJoin ("Block {" : prettify es ++ ["}"])
    (Call f es) -> smartJoin (joinS ["Call", show f, "("] : prettify es ++ [")"])
    (Function t n a r body) ->
        joinS ["Function", show n, show t, "; args", show a, "; returns", show r, "{"]
        : prettify body ++ ["}"]
    (BinaryOp op e1 e2) -> joinOrSplit (joinOrSplit ["BinaryOp " ++ op] e1) e2
    (If eq bl1 bl2) -> addToLast (joinOrSplit ["If"] eq) " {" ++ prettify bl1 ++ ["}", "else {"] ++ prettify bl2 ++ ["}"]
    (While eq bl) -> addToLast (joinOrSplit ["While"] eq) " {" ++ prettify bl ++ ["}"]
