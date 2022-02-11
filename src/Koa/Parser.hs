module Koa.Parser
  ( ParserConfig (..),
    parseProgram,
  )
where

import Koa.Syntax (Program)

-- | Parser configuration.
data ParserConfig = ParserConfig
  {
  }
  deriving (Show, Eq)

-- | Parse a program from a string.
parseProgram :: ParserConfig -> String -> Either String Program
parseProgram = error "unimplemented parseProgram"
