module Parser where

import Data.Maybe
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

binop = Ex.Infix (BinaryOp <$> op) Ex.AssocLeft
unop = Ex.Prefix (UnaryOp <$> op)

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

opList arity = opList'
  where 
    opList' [op] = [arity op Ex.AssocLeft]
    opList' (op:ops) = arity op Ex.AssocLeft : opList' ops

binList = opList binary

binops = [
   binList ["*", "/", "//", "%"]
 , binList ["+", "-"]
 , binList ["<", "=", "<=", ">=", "==", "!="]
 ]

-- TODO: Unary ops
expr :: Parser Expr
expr =  Ex.buildExpressionParser (binops ++ [[binop]]) factor

exprType :: Parser ExprType
exprType =
  try ( do
    typeID <- identifier
    return $ case typeID of
      "int" -> IntType
      "float" -> FloatType
      "void" -> VoidType
      "bytes" -> BytesType
      "auto" -> AutoType )
  <|> try ( parens $ do
    fromTypes <- commaSep exprType
    reserved "->"
    CallableType fromTypes <$> exprType )

int :: Parser Expr
int = Int <$> integer

floating :: Parser Expr
floating = Float <$> float

variable :: Parser Expr
variable = Var <$> identifier

definition :: Parser Expr
definition = do
  varType <- exprType
  whitespace
  varName <- identifier
  return $ Def varType varName

codeBlock :: Parser [Expr]
codeBlock = braces $ many $
  do e <- expr
     reserved ";"
     return e

block :: Parser Expr
block = Block <$> codeBlock

decorator :: Parser Modifier
decorator = do
  char '@'
  Decorator <$> identifier

modifiers :: Parser [Modifier]
modifiers = do
  decs <- many decorator
  -- TODO: in future here we can add parsing of some other modifiers
  return decs

function :: Parser Expr
function = do
  mods <- modifiers
  funcType <- exprType
  name <- identifier
  args <- parens $ commaSep definition
  mReturns <- optionMaybe $ do
    reserved "returns"
    id <- identifier
    return id
  body <- do
    reserved "="
    block <- optionMaybe codeBlock
    case block of
      Just codeBlock -> return codeBlock
      Nothing -> do
        e <- expr
        return [e]
  return $ Function mods funcType name args mReturns body

decoratorDef :: Parser Expr
decoratorDef = do
  char '@'
  funcType <- exprType
  name <- identifier
  reserved "="
  body <- codeBlock
  return $ DecoratorDef funcType name body

decoratorTarget :: Parser Expr
decoratorTarget = reserved "@target" >> return DecoratorTarget

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

ifelse :: Parser Expr
ifelse = do
  reserved "if"
  cond <- parens expr
  tr <- codeBlock
  fl <- optionMaybe $ do
    reserved "else"
    code <- codeBlock
    return code
  return $ If cond tr (fromMaybe [] fl)

while :: Parser Expr
while = do
  reserved "while"
  cond <- parens expr
  body <- codeBlock
  return $ While cond body

cast :: Parser Expr
cast = do
  castedT <- parens exprType
  e <- expr
  return $ TypeCast castedT e
 
factor :: Parser Expr
factor = try cast
      <|> try block
      <|> try function
      <|> try floating
      <|> try int
      <|> try call
      <|> try definition
      <|> try decoratorTarget
      <|> try variable
      <|> try ifelse
      <|> try while
      <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- try function
       <|> try decoratorDef
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseCode :: String -> Either ParseError AST
parseCode s = parse (contents toplevel) "<stdin>" s
