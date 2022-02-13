{-# LANGUAGE TypeFamilies #-}
module Koa.Parser
  ( ParserConfig (..),
    parseProgram,
  )
where

import Koa.Syntax
import Control.Monad.Parser

-- | Parser configuration.
data ParserConfig = ParserConfig
  {
  }
  deriving (Show, Eq)

lexeme :: CharParser p => p a -> p a
lexeme p = spaces *> p <* spaces
  where
    spaces = many $ oneOf " \n\r\t"

letter :: CharParser p => p Char
letter = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"

ident :: CharParser p => p Ident
ident = lexeme $ Ident <$> many1 letter

-- TODO: Parse the return type and the expression blocks
definition :: CharParser p => p Definition
definition = do
  name <- ident
  return $ DFn name [] TEmpty (BExpr [] $ Expr $ ELit LEmpty)

program :: CharParser p => p Program
program = Program <$> many (lexeme definition)

-- | Parse a program from a string.
parseProgram :: ParserConfig -> String -> Either String Program
parseProgram _ [] = Right $ Program []
parseProgram _ a = case runStringParser (program <* eof) a of
  Parsed v _ _ -> Right v
  _ -> Left "Parse error"
