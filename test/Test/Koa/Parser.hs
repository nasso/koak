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
        "fn main(): () { }"
        ( Program
            [ DFn
                (Ident "main")
                []
                TEmpty
                (BExpr [] $ Expr $ ELit LEmpty)
            ]
        ),
    testCase "main returning empty" $
      assertProgram
        "fn main(): () {\n\
        \  ()\n\
        \}\n"
        ( Program
            [ DFn
                (Ident "main")
                []
                TEmpty
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
        ),
    testCase "function returning a binary operator" $
      assertProgram
        "fn test(): i32 {\n\
        \  1 + 2\n\
        \}\n"
        ( Program
            [ DFn
                (Ident "test")
                []
                TInt32
                ( BExpr [] $
                    Expr $
                      EBinop
                        OAdd
                        (Expr $ ELit $ LInt 1)
                        (Expr $ ELit $ LInt 2)
                )
            ]
        ),
    testCase "function returning a binary operator using a variable" $
      assertProgram
        "fn test(): i32 {\n\
        \  a + b\n\
        \}\n"
        ( Program
            [ DFn
                (Ident "test")
                []
                TInt32
                ( BExpr [] $
                    Expr $
                      EBinop
                        OAdd
                        (Expr $ EIdent (Ident "a"))
                        (Expr $ EIdent (Ident "b"))
                )
            ]
        ),
    testCase "simple addition expression" $
      assertExpr "a + b" $
        Expr $
          EBinop
            OAdd
            (Expr $ EIdent $ Ident "a")
            (Expr $ EIdent $ Ident "b"),
    testCase "simple subtraction expression" $
      assertExpr "a - b" $
        Expr $
          EBinop
            OSub
            (Expr $ EIdent $ Ident "a")
            (Expr $ EIdent $ Ident "b"),
    testCase "simple multiplication expression" $
      assertExpr "a * b" $
        Expr $
          EBinop
            OMul
            (Expr $ EIdent $ Ident "a")
            (Expr $ EIdent $ Ident "b"),
    testCase "simple division expression" $
      assertExpr "a / b" $
        Expr $
          EBinop
            ODiv
            (Expr $ EIdent $ Ident "a")
            (Expr $ EIdent $ Ident "b"),
    testCase "simple not equal expression" $
      assertExpr "a != b" $
        Expr $
          EBinop
            ONotEquals
            (Expr $ EIdent $ Ident "a")
            (Expr $ EIdent $ Ident "b"),
    testCase "simple equal expression" $
      assertExpr "a == b" $
        Expr $
          EBinop
            OEquals
            (Expr $ EIdent $ Ident "a")
            (Expr $ EIdent $ Ident "b"),
    testCase "simple greater than expression" $
      assertExpr "a > b" $
        Expr $
          EBinop
            OGreaterThan
            (Expr $ EIdent $ Ident "a")
            (Expr $ EIdent $ Ident "b"),
    testCase "simple greater than or equal expression" $
      assertExpr "a >= b" $
        Expr $
          EBinop
            OGreaterThanEq
            (Expr $ EIdent $ Ident "a")
            (Expr $ EIdent $ Ident "b"),
    testCase "simple less than expression" $
      assertExpr "a < b" $
        Expr $
          EBinop
            OLessThan
            (Expr $ EIdent $ Ident "a")
            (Expr $ EIdent $ Ident "b"),
    testCase "simple less than or equal expression" $
      assertExpr "a <= b" $
        Expr $
          EBinop
            OLessThanEq
            (Expr $ EIdent $ Ident "a")
            (Expr $ EIdent $ Ident "b"),
    testCase "simple multiple add expression" $
      assertExpr "a + b + c" $
        Expr $
          EBinop
            OAdd
            ( Expr $
                EBinop
                  OAdd
                  (Expr $ EIdent $ Ident "a")
                  (Expr $ EIdent $ Ident "b")
            )
            (Expr $ EIdent $ Ident "c"),
    testCase "simple priority mul then add expressions" $
      assertExpr "a * b + c" $
        Expr $
          EBinop
            OAdd
            ( Expr $
                EBinop
                  OMul
                  (Expr $ EIdent $ Ident "a")
                  (Expr $ EIdent $ Ident "b")
            )
            (Expr $ EIdent $ Ident "c"),
    testCase "simple priority add then mul expressions" $
      assertExpr "a + b * c" $
        Expr $
          EBinop
            OAdd
            (Expr $ EIdent $ Ident "a")
            ( Expr $
                EBinop
                  OMul
                  (Expr $ EIdent $ Ident "b")
                  (Expr $ EIdent $ Ident "c")
            ),
    testCase "simple priority add then div expressions" $
      assertExpr "a + b / c" $
        Expr $
          EBinop
            OAdd
            (Expr $ EIdent $ Ident "a")
            ( Expr $
                EBinop
                  ODiv
                  (Expr $ EIdent $ Ident "b")
                  (Expr $ EIdent $ Ident "c")
            ),
    testCase "simple priority add then sub expressions" $
      assertExpr "a + b - c" $
        Expr $
          EBinop
            OSub
            ( Expr $
                EBinop
                  OAdd
                  (Expr $ EIdent $ Ident "a")
                  (Expr $ EIdent $ Ident "b")
            )
            (Expr $ EIdent $ Ident "c"),
    testCase "simple priority sub then add expressions" $
      assertExpr "a - b + c" $
        Expr $
          EBinop
            OAdd
            ( Expr $
                EBinop
                  OSub
                  (Expr $ EIdent $ Ident "a")
                  (Expr $ EIdent $ Ident "b")
            )
            (Expr $ EIdent $ Ident "c"),
    testCase "simple priority sub then mul expressions" $
      assertExpr "a - b * c" $
        Expr $
          EBinop
            OSub
            (Expr $ EIdent $ Ident "a")
            ( Expr $
                EBinop
                  OMul
                  (Expr $ EIdent $ Ident "b")
                  (Expr $ EIdent $ Ident "c")
            ),
    testCase "simple priority mul then sub expressions" $
      assertExpr "a * b - c" $
        Expr $
          EBinop
            OSub
            ( Expr $
                EBinop
                  OMul
                  (Expr $ EIdent $ Ident "a")
                  (Expr $ EIdent $ Ident "b")
            )
            (Expr $ EIdent $ Ident "c"),
    testCase "simple priority mul then div expressions" $
      assertExpr "a * b / c" $
        Expr $
          EBinop
            ODiv
            ( Expr $
                EBinop
                  OMul
                  (Expr $ EIdent $ Ident "a")
                  (Expr $ EIdent $ Ident "b")
            )
            (Expr $ EIdent $ Ident "c"),
    testCase "simple priority div then mul expressions" $
      assertExpr "a / b * c" $
        Expr $
          EBinop
            OMul
            ( Expr $
                EBinop
                  ODiv
                  (Expr $ EIdent $ Ident "a")
                  (Expr $ EIdent $ Ident "b")
            )
            (Expr $ EIdent $ Ident "c"),
    testCase "multiple div expressions" $
      assertExpr "a / b / c" $
        Expr $
          EBinop
            ODiv
            ( Expr $
                EBinop
                  ODiv
                  (Expr $ EIdent $ Ident "a")
                  (Expr $ EIdent $ Ident "b")
            )
            (Expr $ EIdent $ Ident "c"),
    testCase "multiple mul expressions" $
      assertExpr "a * b * c" $
        Expr $
          EBinop
            OMul
            ( Expr $
                EBinop
                  OMul
                  (Expr $ EIdent $ Ident "a")
                  (Expr $ EIdent $ Ident "b")
            )
            (Expr $ EIdent $ Ident "c")
  ]

assertProgram :: String -> Program -> Assertion
assertProgram src ast = parseProgram ParserConfig src @?= Right ast

assertExpr :: String -> Expr -> Assertion
assertExpr src ast = parseExpr ParserConfig src @?= Right ast
