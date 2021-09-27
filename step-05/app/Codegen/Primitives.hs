{-# LANGUAGE OverloadedStrings #-}

module Codegen.Primitives where

import Data.Word (Word32)

import LLVM.AST hiding (function, alignment, Call)
import LLVM.AST.AddrSpace
import LLVM.AST.Type as AST
import qualified LLVM.AST as A
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction hiding (load, store)
import LLVM.IRBuilder.Constant hiding (double)
import qualified LLVM.IRBuilder.Instruction as I

import StringUtils

addrSpace :: AddrSpace
addrSpace = AddrSpace 0

iSize :: Word32
iSize = 32

alignment :: Word32
alignment = 4

integerConstant i = ConstantOperand (C.Int {C.integerBits = iSize, C.integerValue = i})

integerPointer :: AST.Type
integerPointer = AST.PointerType i32 addrSpace

allocate :: MonadIRBuilder m => AST.Type -> m Operand
allocate type_ = alloca type_ Nothing alignment

allocateInt :: MonadIRBuilder m => m Operand
allocateInt = allocate i32

load :: MonadIRBuilder m => Operand -> m Operand
load pointer = I.load pointer alignment

store :: MonadIRBuilder m => Operand -> Operand -> m ()
store pointer value = I.store pointer alignment value

saveInt :: MonadIRBuilder m => Integer -> m Operand
saveInt value = do
  pointer <- allocateInt
  store pointer (int32 value)
  return pointer

refName :: String -> A.Name
refName name = Name (toShort' $ name ++ "_0")

globalName :: String -> A.Name
globalName name = Name (toShort' name)

reference :: AST.Type -> String -> Operand
reference type_ name = LocalReference type_ (refName name)

referenceInt :: String -> Operand
referenceInt name = reference i32 name

referenceIntPointer :: String -> Operand
referenceIntPointer name = reference integerPointer name

makeFuncRef :: String -> Operand
makeFuncRef funcName = ConstantOperand (C.GlobalReference funcType $ globalName funcName)
  where funcType = FunctionType i32 [] False

bodyLabel = toShort' "Body"

argName = ("arg_" ++)

intToFloat op = sitofp op double

floatToInt op = fptosi op i32
