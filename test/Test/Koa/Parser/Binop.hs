module Test.Koa.Parser.Binop (binopTests) where

import Koa.Syntax.HIR
import Test.Koa.Parser.Utils
import Test.Koa.Util
import Test.Tasty
import Test.Tasty.HUnit

validSimple :: [TestTree]
validSimple =
  [ testCase "addition expression" $
      assertExpr "a + b" $
        Expr $
          EBinop
            OAdd
            (varI32 "a")
            (varI32 "b"),
    testCase "subtraction expression" $
      assertExpr "a - b" $
        Expr $
          EBinop
            OSub
            (varI32 "a")
            (varI32 "b"),
    testCase "multiplication expression" $
      assertExpr "a * b" $
        Expr $
          EBinop
            OMul
            (varI32 "a")
            (varI32 "b"),
    testCase "division expression" $
      assertExpr "a / b" $
        Expr $
          EBinop
            ODiv
            (varI32 "a")
            (varI32 "b"),
    testCase "not equal expression" $
      assertExpr "a != b" $
        Expr $
          EBinop
            ONotEquals
            (varI32 "a")
            (varI32 "b"),
    testCase "equal expression" $
      assertExpr "a == b" $
        Expr $
          EBinop
            OEquals
            (varI32 "a")
            (varI32 "b"),
    testCase "greater than expression" $
      assertExpr "a > b" $
        Expr $
          EBinop
            OGreaterThan
            (varI32 "a")
            (varI32 "b"),
    testCase "greater than or equal expression" $
      assertExpr "a >= b" $
        Expr $
          EBinop
            OGreaterThanEq
            (varI32 "a")
            (varI32 "b"),
    testCase "less than expression" $
      assertExpr "a < b" $
        Expr $
          EBinop
            OLessThan
            (varI32 "a")
            (varI32 "b"),
    testCase "less than or equal expression" $
      assertExpr "a <= b" $
        Expr $
          EBinop
            OLessThanEq
            (varI32 "a")
            (varI32 "b")
  ]

validPrecedence :: [TestTree]
validPrecedence =
  [ testCase "multiple add expressions" $
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
    testCase "multiple sub expressions" $
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
    testCase "mul then add expressions" $
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
    testCase "add then mul expressions" $
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
    testCase "add then div expressions" $
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
    testCase "add then sub expressions" $
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
    testCase "sub then add expressions" $
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
    testCase "sub then mul expressions" $
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
    testCase "mul then sub expressions" $
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
    testCase "mul then div expressions" $
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
    testCase "div then mul expressions" $
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

simple :: [TestTree]
simple =
  [testGroup "Valid" validSimple]

precedence :: [TestTree]
precedence =
  [testGroup "Valid" validPrecedence]

binopTests :: [TestTree]
binopTests =
  [ testGroup "Simple" simple,
    testGroup "Precedence" precedence
  ]
