module Test.Koa.Parser.Utils (assertProgram, assertExpr) where

import Koa.Parser
import Koa.Syntax
import Test.Tasty.HUnit

assertProgram :: String -> Program -> Assertion
assertProgram src ast = parseProgram ParserConfig src @?= Right ast

assertExpr :: String -> Expr -> Assertion
assertExpr src ast = parseExpr ParserConfig src @?= Right ast
