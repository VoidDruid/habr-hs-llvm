{-# LANGUAGE OverloadedStrings #-}

module Compiler where

import qualified LLVM.AST
import LLVM.Internal.Target (withHostTargetMachineDefault)
import LLVM.Internal.Context (withContext, Context)
import LLVM.Module (withModuleFromAST, writeObjectToFile, Module, File)

writeWithDefaultTarget :: File -> Module -> IO ()
writeWithDefaultTarget file mod = withHostTargetMachineDefault (\t -> writeObjectToFile t file mod)

writeWithModuleFromAST :: File -> Context -> LLVM.AST.Module -> IO ()
writeWithModuleFromAST f c m = withModuleFromAST c m (writeWithDefaultTarget f)

writeObject :: File -> LLVM.AST.Module -> IO ()
writeObject file mod = withContext (\c -> writeWithModuleFromAST file c mod)
