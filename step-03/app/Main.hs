module Main where

import Data.List (intersect)

import System.IO
import System.Environment

import Parser (parseCode)
import Syntax (joinedPrettyAST)
import AST.Processor (processAST)
import AST.Utils (ppAST)
import AST.Errors (showE)

import StringUtils

debugFlag = ["--debug", "-d"]

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
              Left tast -> ppAST tast
      return ()
      where
        (flags, filename) = parseArgs args
        actionFor key action = if not (null (key `intersect` flags))
          then action
          else pure ()
