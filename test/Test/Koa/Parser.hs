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
            (Expr $ EIdent $ Ident "b")
  ]

assertProgram :: String -> Program -> Assertion
assertProgram src ast = parseProgram ParserConfig src @?= Right ast

assertExpr :: String -> Expr -> Assertion
assertExpr src ast = parseExpr ParserConfig src @?= Right ast
