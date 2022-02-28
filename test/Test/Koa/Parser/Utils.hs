module Test.Koa.Parser.Utils (assertProgram, assertExpr, assertError) where

import Koa.Parser
import Koa.Syntax
import Test.Tasty.HUnit

assertProgram :: String -> Program -> Assertion
assertProgram src ast = parseProgram ParserConfig src @?= Right ast

assertExpr :: String -> Expr -> Assertion
assertExpr src ast = parseExpr ParserConfig src @?= Right ast

assertError :: String -> Assertion
assertError src = case parseProgram ParserConfig src of
    Left _ -> return ()
    Right _ -> assertFailure "Expected error"
