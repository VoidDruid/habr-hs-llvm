module Main where

import System.IO
import System.Environment

import Parser (parseCode)
import Syntax (joinedPrettyAST)

main = do
  args <- getArgs
  case args of
    []   -> putStrLn "Provide file name!"
    [filename] -> do
      code <- readFile filename
      case parseCode code of
        Left err -> print err
        Right ast -> putStrLn (joinedPrettyAST ast)
    _ -> putStrLn "Provide one file name!"
