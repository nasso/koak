module Test.Parser (parserTests) where

import Parser
import Syntax
import Test.Tasty
import Test.Tasty.HUnit

parserTests :: [TestTree]
parserTests =
  [ testCase "empty" $ assertProgram "" (Program []),
    testCase "minimal" $
      assertProgram
        "fn main(): i32 {\n\
        \  0\n\
        \}\n"
        ( Program
            [ DFn
                (Ident "main")
                []
                TInt32
                (BExpr [] $ Just $ Expr $ ELit $ LInt 0)
            ]
        )
  ]

assertProgram :: String -> Program -> Assertion
assertProgram src ast = parseProgram ParserConfig src @?= Right ast
