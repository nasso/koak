module Test.Koa.Parser.Utils (assertProgram, assertExpr, assertError) where

import Koa.Parser
import Koa.Syntax.HIR
import Test.Tasty.HUnit

assertProgram :: String -> Program -> Assertion
assertProgram src ast = parseProgram ParserConfig src @?= Right ast

assertExpr :: String -> Expr -> Assertion
assertExpr src ast = parseExpr ParserConfig src @?= Right ast

assertError :: String -> Assertion
assertError src =
  case parseProgram ParserConfig src of
    Left _ -> pure ()
    Right _ -> assertFailure "Expected error"
