module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (many)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = [ "+", "*", "-", "/", "//", "%"
          , "<", ">", ">=", "<=", "==", "!="
          , ";", "=", "," , "->"]
    names = ["if", "else", "while", "for", "returns", "@target", "@args"]
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

operator :: Parser String
operator = do
  c <- Tok.opStart emptyDef
  cs <- many $ Tok.opLetter emptyDef
  return (c:cs)
