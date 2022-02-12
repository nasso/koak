module Test.Koa.Parser (parserTests) where

import Koa.Parser
import Koa.Syntax
import Test.Tasty
import Test.Tasty.HUnit

parserTests :: [TestTree]
parserTests =
  [ testCase "empty" $ assertProgram "" (Program []),
    testCase "empty main" $
      assertProgram
        "fn main(): i32 { }"
        ( Program
            [ DFn
                (Ident "main")
                []
                TInt32
                (BExpr [] $ Expr $ ELit LEmpty)
            ]
        ),
    testCase "main returning empty" $
      assertProgram
        "fn main(): i32 {\n\
        \  ()\n\
        \}\n"
        ( Program
            [ DFn
                (Ident "main")
                []
                TInt32
                (BExpr [] $ Expr $ ELit LEmpty)
            ]
        ),
    testCase "main returning zero" $
      assertProgram
        "fn main(): i32 {\n\
        \  0\n\
        \}\n"
        ( Program
            [ DFn
                (Ident "main")
                []
                TInt32
                (BExpr [] $ Expr $ ELit $ LInt 0)
            ]
        )
  ]

assertProgram :: String -> Program -> Assertion
assertProgram src ast = parseProgram ParserConfig src @?= Right ast
