module Main where

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = [ "+", "*", "-", "/"]
    names = ["if", "else"]
    style = emptyDef {
               Tok.commentLine = "//"
             , Tok.commentStart = "/*"
             , Tok.commentEnd = "*/"
             , Tok.caseSensitive = True
             , Tok.reservedOpNames = ops
             , Tok.reservedNames = names
             }

integer    = Tok.integer lexer
float      = Tok.float lexer
parens     = Tok.parens lexer
braces     = Tok.braces lexer
commaSep   = Tok.commaSep lexer
semiSep    = Tok.semiSep lexer
identifier = Tok.identifier lexer
whitespace = Tok.whiteSpace lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer

sumParse :: Parser Integer
sumParse = do
  first <- integer
  reservedOp "+"
  second <- integer
  return (first + second)

runSumParse = parse sumParse "<stdin>"

main = do
  line <- getLine
  case runSumParse line of
    Right result -> putStrLn ("Result: " ++ show result)
    Left error -> print error
