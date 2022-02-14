{-# LANGUAGE TypeFamilies #-}

module Koa.Parser
  ( ParserConfig (..),
    parseProgram,
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

-- | Parse a program from a string.
parseProgram :: ParserConfig -> String -> Either String Program
parseProgram _ [] = Right $ Program []
parseProgram _ a =
  case runStringParser (whitespace *> program <* eof) a of
    Parsed v _ _ -> Right v
    NoParse err -> Left $ show err

program :: CharParser p => p Program
program = Program <$> many (lexeme definition)

-- TODO: Parse the return type and the expression blocks
definition :: CharParser p => p Definition
definition =
  do
    name <- ident
    pure $ DFn name [] TEmpty (BExpr [] $ Expr $ ELit LEmpty)

ident :: CharParser p => p Ident
ident = lexeme $ Ident <$> ((:) <$> alpha <*> many alphanum) <?> "identifier"

alpha :: CharParser p => p Char
alpha = match isAlpha

alphanum :: CharParser p => p Char
alphanum = match isAlphaNum

lexeme :: CharParser p => p a -> p a
lexeme p = p <* whitespace

whitespace :: CharParser p => p ()
whitespace = void $ many $ match isSpace
