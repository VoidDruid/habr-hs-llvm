module Main where

import Data.List (intersect)
import qualified Data.Text.Lazy.IO as TLIO

import System.IO
import System.Environment

import LLVM.Pretty (ppllvm)

import Parser (parseCode)
import Syntax (joinedPrettyAST)
import AST.Processor (processAST)
import AST.Utils (ppAST)
import AST.Errors (showE)
import Codegen.Builder (buildIR)

import StringUtils

debugFlag = ["--debug", "-d"]
emitFlag = ["--emit", "-e"]

parseArgs :: [String] -> ([String], String) -- [flags], filename
parseArgs (reverse -> (filename:args)) = (args, filename) -- TODO: parse incoming arguments

main = do
  args <- getArgs
  case args of
    []   -> putStrLn "Provide file name!"
    args -> do
      code <- readFile filename
      case parseCode code of
        Left err -> print err
        Right ast -> do
            actionFor debugFlag (ppAST ast >> putStrLn "")
            let maybeTAST = processAST ast
            case maybeTAST of
              Right errors -> putStrLn $ joinN (map showE errors)
              Left tast -> do
                actionFor debugFlag (ppAST tast >> putStrLn "")
                let ir = buildIR tast
                actionFor emitFlag (TLIO.putStrLn $ ppllvm ir)
      return ()
      where
        (flags, filename) = parseArgs args
        actionFor key action = if not (null (key `intersect` flags))
          then action
          else pure ()
