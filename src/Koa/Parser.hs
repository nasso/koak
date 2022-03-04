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
    <*> ((symbol ":" >> type') <|> pure TEmpty)
    <*> block

type' :: CharParser p => p Type
type' =
  TEmpty <$ (symbol "(" >> symbol ")")
    <|> TInt32 <$ symbol "i32"
    <|> TFloat64 <$ symbol "f64"

block :: CharParser p => p Block
block = braces $ BExpr <$> many stmt <*> optional expr

prio :: CharParser p => p Expr
prio = chainl1 term binopExprPrio

term :: CharParser p => p Expr
term =
  parserFor
    <|> parserWhile
    <|> parserIf
    <|> parserCall
    <|> parseAssign
    <|> (Expr . ELit <$> literal)
    <|> (Expr . EIdent <$> ident)

expr :: CharParser p => p Expr
expr = chainl1 prio binopExpr <|> term

mutIdent :: CharParser p => p Pattern
mutIdent = PMutIdent <$> (symbol "mut" *> ident)

wildcard :: CharParser p => p Pattern
wildcard = PWildcard <$ symbol "_"

pattern' :: CharParser p => p Pattern
pattern' = wildcard <|> mutIdent <|> PIdent <$> ident

letType :: CharParser p => p Type
letType = symbol ":" *> type'

letStmt :: CharParser p => p Stmt
letStmt =
  SLet
    <$> (symbol "let" *> pattern') <*> optional letType <*> (symbol "=" *> expr)

stmt :: CharParser p => p Stmt
stmt =
  SReturn <$> (symbol "return" *> expr <* symbol ";")
    <|> letStmt <* symbol ";"
    <|> SExpr <$> expr <* symbol ";"

binopExpr :: CharParser p => p (Expr -> Expr -> Expr)
binopExpr =
  do
    op <- binop
    pure $ \l r -> Expr $ EBinop op l r

binopExprPrio :: CharParser p => p (Expr -> Expr -> Expr)
binopExprPrio =
  do
    op <- binopPrio
    pure $ \l r -> Expr $ EBinop op l r

binopPrio :: CharParser p => p Binop
binopPrio =
  OMul <$ symbol "*"
    <|> ODiv <$ symbol "/"

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
ident =
  lexeme $
    Ident
      <$> ( ((:) <$> initial <*> many subseq)
              <|> ((:) <$> like '_' <*> many1 alphanum)
          )
      <?> "identifier"
  where
    initial = alpha
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

parserFor :: CharParser p => p Expr
parserFor =
  do
    symbol "for"
    i <- letStmt <* symbol ","
    e <- expr <* symbol ","
    e' <- expr
    Expr . EFor i e e' <$> block

parseAssign :: CharParser p => p Expr
parseAssign =
  do
    a <- ident
    symbol "="
    Expr . EAssign a <$> expr

parserWhile :: CharParser p => p Expr
parserWhile = do
  symbol "while"
  e <- expr
  Expr . EWhile e <$> block

parserIf :: CharParser p => p Expr
parserIf = parserIfElse <|> onlyIf

onlyIf :: CharParser p => p Expr
onlyIf =
  do
    symbol "if"
    e <- expr
    b <- block
    Expr . EIf e b <$> emptyBlock

emptyBlock :: CharParser p => p Block
emptyBlock = pure $ BExpr [] Nothing

parserIfElse :: CharParser p => p Expr
parserIfElse =
  do
    symbol "if"
    e <- expr
    b <- block
    symbol "else"
    Expr . EIf e b <$> block

parserCall :: CharParser p => p Expr
parserCall =
  do
    i <- ident
    args <- parens $ sepBy expr (symbol ",")
    pure $ Expr $ ECall i args
