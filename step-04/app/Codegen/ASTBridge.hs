module Codegen.ASTBridge where

import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map

import LLVM.AST hiding (VoidType)
import LLVM.AST.Type as AST hiding (VoidType)

import LLVM.IRBuilder.Constant hiding (double)
import LLVM.IRBuilder.Instruction hiding (load, store)
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Module

import qualified LLVM.AST.IntegerPredicate as IPredicats
import qualified LLVM.AST.FloatingPointPredicate as FPredicats

import Syntax hiding (AST)
import Codegen.Primitives
import StringUtils

typeMap = Map.fromList 
  [ (IntType, i32)
  , (FloatType, double)
  , (VoidType, void)
  ]

toLLVMType = (typeMap !)

argDef (TypedExpr defType (TDef name)) = (toLLVMType defType, ParameterName $ toShort' (argName name))

allocateDef (TypedExpr defType (TDef name)) = allocate (toLLVMType defType) `named` toShort' name

allocateT :: MonadIRBuilder m => ExprType -> m Operand
allocateT type_ = allocate (toLLVMType type_)

pointerTo type_ = AST.PointerType (toLLVMType type_) addrSpace

referenceVar :: ExprType -> String -> Operand
referenceVar varType = reference (pointerTo varType)

referenceLocal :: ExprType -> String -> Operand
referenceLocal varType = reference (toLLVMType varType)

convert from to op
      | to == FloatType && from == IntType = intToFloat op
      | to == IntType && from == FloatType = floatToInt op
      | otherwise = return op

cmpOps = [">", "<", "==", "!=", "<=", ">="]

-- TODO: %, //
opTable :: MonadIRBuilder m => Map.Map ExprType (Map.Map String (Operand -> Operand -> m Operand))
opTable = Map.fromList [(IntType, intMap), (FloatType, floatMap)]
  where
    [intMap, floatMap] = map Map.fromList
      [ [ ("+", add)
        , ("-", sub)
        , ("*", mul)
        , ("<", icmp IPredicats.SLT)
        , (">", icmp IPredicats.SGT)
        , ("==", icmp IPredicats.EQ)
        , ("!=", icmp IPredicats.NE)
        , ("<=", icmp IPredicats.SLE)
        , (">=", icmp IPredicats.SGE)
        ]
      , [ ("+", fadd)
        , ("-", fsub)
        , ("*", fmul)
        , ("<", fcmp FPredicats.OLT)
        , (">", fcmp FPredicats.OGT)
        , ("==", fcmp FPredicats.OEQ)
        , ("!=", fcmp FPredicats.ONE)
        , ("<=", fcmp FPredicats.OLE)
        , (">=", fcmp FPredicats.OGE)
        ]
      ]

findOperation type_ op = (opTable ! type_) ! op
