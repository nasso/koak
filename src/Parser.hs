module Parser
  ( ParserConfig,
    parseProgram,
  )
where

import Syntax (Program)

-- | Parser configuration.
data ParserConfig = ParserConfig
  {
  }
  deriving (Show, Eq)

-- | Parse a program from a string.
parseProgram :: ParserConfig -> String -> Either String Program
parseProgram = error "unimplemented parseProgram"
