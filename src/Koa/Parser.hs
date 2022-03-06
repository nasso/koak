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
import Koa.Syntax.HIR

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
    <*> parens (sepBy typedPatternBinding $ symbol ",")
    <*> ((symbol ":" >> type') <|> pure TEmpty)
    <*> block

typedPatternBinding :: CharParser p => p TBinding
typedPatternBinding = TBinding <$> pattern' <*> (symbol ":" >> type')

type' :: CharParser p => p Type
type' =
  TEmpty <$ (symbol "(" >> symbol ")")
    <|> TInt32 <$ symbol "i32"
    <|> TFloat64 <$ symbol "f64"
    <|> TBool <$ symbol "bool"

block :: CharParser p => p Block
block = braces $ BExpr <$> many stmt <*> optional expr

term :: CharParser p => p Expr
term =
  parseUnop
    <|> parserFor
    <|> parserWhile
    <|> parserIf
    <|> parserCall
    <|> parseAssign
    <|> (Expr . ELit <$> literal)
    <|> (Expr . EIdent <$> ident)
    <|> (Expr . EBlock <$> block)

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

returnStmt :: CharParser p => p Stmt
returnStmt =
  SReturn
    <$> ( symbol "return"
            *> ( expr
                   <|> pure (Expr (ELit LEmpty))
               )
            <* symbol ";"
        )

stmt :: CharParser p => p Stmt
stmt =
  returnStmt
    <|> letStmt <* symbol ";"
    <|> SExpr <$> expr <* symbol ";"

expr :: CharParser p => p Expr
expr = parseTerm80

-- Unary operators
parsePos :: CharParser p => p (Expr -> Expr)
parsePos = id <$ symbol "+"

parseNeg :: CharParser p => p (Expr -> Expr)
parseNeg = (Expr . EUnop ONeg) <$ symbol "-"

parseNot :: CharParser p => p (Expr -> Expr)
parseNot = (Expr . EUnop ONot) <$ symbol "!"

parseUnop :: CharParser p => p Expr
parseUnop = (parsePos <|> parseNeg <|> parseNot) <*> term

-- Binary operators precedence
-- 80 -> Equality
parseEq :: CharParser p => p (Expr -> Expr -> Expr)
parseEq = (\l r -> Expr $ EBinop OEquals l r) <$ symbol "=="

parseNe :: CharParser p => p (Expr -> Expr -> Expr)
parseNe = (\l r -> Expr $ EBinop ONotEquals l r) <$ symbol "!="

parseLe :: CharParser p => p (Expr -> Expr -> Expr)
parseLe = (\l r -> Expr $ EBinop OLessThanEq l r) <$ symbol "<="

parseGe :: CharParser p => p (Expr -> Expr -> Expr)
parseGe = (\l r -> Expr $ EBinop OGreaterThanEq l r) <$ symbol ">="

parseLt :: CharParser p => p (Expr -> Expr -> Expr)
parseLt = (\l r -> Expr $ EBinop OLessThan l r) <$ symbol "<"

parseGt :: CharParser p => p (Expr -> Expr -> Expr)
parseGt = (\l r -> Expr $ EBinop OGreaterThan l r) <$ symbol ">"

parseTerm80 :: CharParser p => p Expr
parseTerm80 =
  parseTerm95
    `chainl1` ( parseEq
                  <|> parseNe
                  <|> parseLe
                  <|> parseGe
                  <|> parseLt
                  <|> parseGt
              )

-- 95 -> Additive
parseAdd :: CharParser p => p (Expr -> Expr -> Expr)
parseAdd = (\l r -> Expr $ EBinop OAdd l r) <$ symbol "+"

parseSub :: CharParser p => p (Expr -> Expr -> Expr)
parseSub = (\l r -> Expr $ EBinop OSub l r) <$ symbol "-"

parseTerm95 :: CharParser p => p Expr
parseTerm95 = parseTerm100 `chainl1` (parseAdd <|> parseSub)

-- 100 -> Multiplicative
parseMul :: CharParser p => p (Expr -> Expr -> Expr)
parseMul = (\l r -> Expr $ EBinop OMul l r) <$ symbol "*"

parseDiv :: CharParser p => p (Expr -> Expr -> Expr)
parseDiv = (\l r -> Expr $ EBinop ODiv l r) <$ symbol "/"

parseTerm100 :: CharParser p => p Expr
parseTerm100 = term `chainl1` (parseMul <|> parseDiv)

literal :: CharParser p => p Literal
literal =
  LFloat <$> try floating
    <|> LInt <$> try integer
    <|> LEmpty <$ (symbol "(" >> symbol ")")
    <|> LBool True <$ symbol "true"
    <|> LBool False <$ symbol "false"

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
