{-# LANGUAGE TypeFamilies #-}

module Koa.Parser
  ( ParserConfig (..),
    parseProgram,
  )
where

import Control.Monad.Parser
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
  case runStringParser (program <* eof) a of
    Parsed v _ _ -> Right v
    _ -> Left "Parse error"

program :: CharParser p => p Program
program = Program <$> many (lexeme definition)

-- TODO: Parse the return type and the expression blocks
definition :: CharParser p => p Definition
definition = do
  name <- ident
  return $ DFn name [] TEmpty (BExpr [] $ Expr $ ELit LEmpty)

ident :: CharParser p => p Ident
ident = lexeme $ Ident <$> many1 letter

letter :: CharParser p => p Char
letter = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"

lexeme :: CharParser p => p a -> p a
lexeme p = spaces *> p <* spaces
  where
    spaces = many $ oneOf " \n\r\t"
