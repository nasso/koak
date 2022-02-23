{-# LANGUAGE TypeFamilies #-}

module Koa.Parser
  ( ParserConfig (..),
    parseProgram,
    parseExpr,
  )
where

import Control.Monad
import Control.Monad.Parser
import Data.Char
import Koa.Syntax

-- | Parser configuration.
data ParserConfig = ParserConfig
  {
  }
  deriving (Show, Eq)

-- | Parse an expression from a string.
parseExpr :: ParserConfig -> String -> Either String Expr
parseExpr _ a =
  case runStringParser (optional whitespace *> expr <* eof) a of
    Parsed v _ _ -> Right v
    NoParse err -> Left $ show err

-- | Parse a program from a string.
parseProgram :: ParserConfig -> String -> Either String Program
parseProgram _ [] = Right $ Program []
parseProgram _ a =
  case runStringParser (optional whitespace *> program <* eof) a of
    Parsed v _ _ -> Right v
    NoParse err -> Left $ show err

-- | Parser for a program.
program :: CharParser p => p Program
program = Program <$> many (lexeme definition)

-- | Parser for a definition.
definition :: CharParser p => p Definition
definition =
  DFn
    <$> (symbol "fn" >> ident)
    <*> parens (pure [])
    <*> (symbol ":" >> type')
    <*> block

type' :: CharParser p => p Type
type' =
  TEmpty <$ (symbol "(" >> symbol ")")
    <|> TInt32 <$ symbol "i32"
    <|> TFloat64 <$ symbol "f64"

block :: CharParser p => p Block
block = BExpr [] <$> braces (expr <|> pure (Expr $ ELit LEmpty))

term :: CharParser p => p Expr
term =
  (Expr . ELit <$> literal)
    <|> (Expr . EIdent <$> ident)

expr :: CharParser p => p Expr
expr = chainl1 term binopExpr <|> term

binopExpr :: CharParser p => p (Expr -> Expr -> Expr)
binopExpr =
  do
    op <- binop
    pure $ \l r -> Expr $ EBinop op l r

-- | Parser for binary operators.
binop :: CharParser p => p Binop
binop =
  OEquals <$ symbol "=="
    <|> ONotEquals <$ symbol "!="
    <|> OGreaterThanEq <$ symbol ">="
    <|> OLessThanEq <$ symbol "<="
    <|> OGreaterThan <$ symbol ">"
    <|> OLessThan <$ symbol "<"
    <|> OAdd <$ symbol "+"
    <|> OSub <$ symbol "-"
    <|> OMul <$ symbol "*"
    <|> ODiv <$ symbol "/"

literal :: CharParser p => p Literal
literal =
  LFloat <$> try floating
    <|> LInt <$> try integer
    <|> LEmpty <$ (symbol "(" >> symbol ")")

integer :: CharParser p => p Integer
integer = lexeme $ read <$> many1 digit

floating :: CharParser p => p Double
floating =
  lexeme $ do
    n <- many1 digit
    f <- (:) <$> like '.' <*> many1 digit
    pure $ read $ n ++ f

ident :: CharParser p => p Ident
ident = lexeme $ Ident <$> ((:) <$> initial <*> many subseq) <?> "identifier"
  where
    initial = alpha <|> like '_'
    subseq = alphanum <|> like '_'

alpha :: CharParser p => p Char
alpha = match isAlpha

digit :: CharParser p => p Char
digit = match isDigit

alphanum :: CharParser p => p Char
alphanum = match isAlphaNum

parens :: CharParser p => p a -> p a
parens p = symbol "(" *> p <* symbol ")"

braces :: CharParser p => p a -> p a
braces p = symbol "{" *> p <* symbol "}"

symbol :: CharParser p => String -> p ()
symbol s = void $ lexeme (string s) <?> "\"" ++ s ++ "\""

lexeme :: CharParser p => p a -> p a
lexeme p = p <* optional whitespace

whitespace :: CharParser p => p ()
whitespace = void $ some $ match isSpace
