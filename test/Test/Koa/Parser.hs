module Test.Koa.Parser (parserTests) where

import Koa.Parser
import Koa.Syntax
import Test.Koa.Util
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
                (BExpr [] Nothing)
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
                (BExpr [] $ Just litEmpty)
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
                (BExpr [] $ Just $ litI32 0)
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
                    Just $
                      Expr $
                        EBinop
                          OAdd
                          (litI32 1)
                          (litI32 2)
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
                    Just $
                      Expr $
                        EBinop
                          OAdd
                          (varI32 "a")
                          (varI32 "b")
                )
            ]
        ),
    testCase "simple addition expression" $
      assertExpr "a + b" $
        Expr $
          EBinop
            OAdd
            (varI32 "a")
            (varI32 "b"),
    testCase "simple subtraction expression" $
      assertExpr "a - b" $
        Expr $
          EBinop
            OSub
            (varI32 "a")
            (varI32 "b"),
    testCase "simple multiplication expression" $
      assertExpr "a * b" $
        Expr $
          EBinop
            OMul
            (varI32 "a")
            (varI32 "b"),
    testCase "simple division expression" $
      assertExpr "a / b" $
        Expr $
          EBinop
            ODiv
            (varI32 "a")
            (varI32 "b"),
    testCase "simple not equal expression" $
      assertExpr "a != b" $
        Expr $
          EBinop
            ONotEquals
            (varI32 "a")
            (varI32 "b"),
    testCase "simple equal expression" $
      assertExpr "a == b" $
        Expr $
          EBinop
            OEquals
            (varI32 "a")
            (varI32 "b"),
    testCase "simple greater than expression" $
      assertExpr "a > b" $
        Expr $
          EBinop
            OGreaterThan
            (varI32 "a")
            (varI32 "b"),
    testCase "simple greater than or equal expression" $
      assertExpr "a >= b" $
        Expr $
          EBinop
            OGreaterThanEq
            (varI32 "a")
            (varI32 "b"),
    testCase "simple less than expression" $
      assertExpr "a < b" $
        Expr $
          EBinop
            OLessThan
            (varI32 "a")
            (varI32 "b"),
    testCase "simple less than or equal expression" $
      assertExpr "a <= b" $
        Expr $
          EBinop
            OLessThanEq
            (varI32 "a")
            (varI32 "b"),
    testCase "simple multiple add expression" $
      assertExpr "a + b + c" $
        Expr $
          EBinop
            OAdd
            ( Expr $
                EBinop
                  OAdd
                  (varI32 "a")
                  (varI32 "b")
            )
            (varI32 "c"),
    testCase "simple multiple sub expressions" $
      assertExpr "a - b - c" $
        Expr $
          EBinop
            OSub
            ( Expr $
                EBinop
                  OSub
                  (varI32 "a")
                  (varI32 "b")
            )
            (varI32 "c"),
    testCase "simple priority mul then add expressions" $
      assertExpr "a * b + c" $
        Expr $
          EBinop
            OAdd
            ( Expr $
                EBinop
                  OMul
                  (varI32 "a")
                  (varI32 "b")
            )
            (varI32 "c"),
    testCase "simple priority add then mul expressions" $
      assertExpr "a + b * c" $
        Expr $
          EBinop
            OAdd
            (varI32 "a")
            ( Expr $
                EBinop
                  OMul
                  (varI32 "b")
                  (varI32 "c")
            ),
    testCase "simple priority add then div expressions" $
      assertExpr "a + b / c" $
        Expr $
          EBinop
            OAdd
            (varI32 "a")
            ( Expr $
                EBinop
                  ODiv
                  (varI32 "b")
                  (varI32 "c")
            ),
    testCase "simple priority add then sub expressions" $
      assertExpr "a + b - c" $
        Expr $
          EBinop
            OSub
            ( Expr $
                EBinop
                  OAdd
                  (varI32 "a")
                  (varI32 "b")
            )
            (varI32 "c"),
    testCase "simple priority sub then add expressions" $
      assertExpr "a - b + c" $
        Expr $
          EBinop
            OAdd
            ( Expr $
                EBinop
                  OSub
                  (varI32 "a")
                  (varI32 "b")
            )
            (varI32 "c"),
    testCase "simple priority sub then mul expressions" $
      assertExpr "a - b * c" $
        Expr $
          EBinop
            OSub
            (varI32 "a")
            ( Expr $
                EBinop
                  OMul
                  (varI32 "b")
                  (varI32 "c")
            ),
    testCase "simple priority mul then sub expressions" $
      assertExpr "a * b - c" $
        Expr $
          EBinop
            OSub
            ( Expr $
                EBinop
                  OMul
                  (varI32 "a")
                  (varI32 "b")
            )
            (varI32 "c"),
    testCase "simple priority mul then div expressions" $
      assertExpr "a * b / c" $
        Expr $
          EBinop
            ODiv
            ( Expr $
                EBinop
                  OMul
                  (varI32 "a")
                  (varI32 "b")
            )
            (varI32 "c"),
    testCase "simple priority div then mul expressions" $
      assertExpr "a / b * c" $
        Expr $
          EBinop
            OMul
            ( Expr $
                EBinop
                  ODiv
                  (varI32 "a")
                  (varI32 "b")
            )
            (varI32 "c"),
    testCase "multiple div expressions" $
      assertExpr "a / b / c" $
        Expr $
          EBinop
            ODiv
            ( Expr $
                EBinop
                  ODiv
                  (varI32 "a")
                  (varI32 "b")
            )
            (varI32 "c"),
    testCase "multiple mul expressions" $
      assertExpr "a * b * c" $
        Expr $
          EBinop
            OMul
            ( Expr $
                EBinop
                  OMul
                  (varI32 "a")
                  (varI32 "b")
            )
            (varI32 "c")
  ]

assertProgram :: String -> Program -> Assertion
assertProgram src ast = parseProgram ParserConfig src @?= Right ast

assertExpr :: String -> Expr -> Assertion
assertExpr src ast = parseExpr ParserConfig src @?= Right ast
