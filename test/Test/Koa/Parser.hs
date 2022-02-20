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
    testCase "test function returning a Binop" $
      assertProgram
        "fn test(): i32 {\n\
        \  1 + 2\n\
        \}\n"
        ( Program
            [ DFn
                (Ident "test")
                []
                TInt32
                (BExpr [] $ Expr $ EBinop
                  OAdd (Expr $ ELit $ LInt 1) (Expr $ ELit $ LInt 2))
            ]
        ),
      testCase "test function returning a Binop using a variable" $
        assertProgram
          "fn test(): i32 {\n\
          \  a + b\n\
          \}\n"
          ( Program
              [ DFn
                  (Ident "test")
                  []
                  TInt32
                  (BExpr [] $ Expr $ EBinop
                    OAdd (Expr $ EIdent (Ident "a")) (Expr $ EIdent (Ident "b")))
              ]
          ),
      testCase "simple addition" $
        assertExpr "1 + 2" $
          Expr $ EBinop
            OAdd
            (Expr $ ELit $ LInt 1)
            (Expr $ ELit $ LInt 2),
      testCase "simple subtraction" $
        assertExpr "1 - 2" $
          Expr $ EBinop
            OSub
            (Expr $ ELit $ LInt 1)
            (Expr $ ELit $ LInt 2),
      testCase "simple multiplication" $
        assertExpr "1 * 2" $
          Expr $ EBinop
            OMul
            (Expr $ ELit $ LInt 1)
            (Expr $ ELit $ LInt 2),
      testCase "simple division" $
        assertExpr "1 / 2" $
          Expr $ EBinop
            ODiv
            (Expr $ ELit $ LInt 1)
            (Expr $ ELit $ LInt 2),
      testCase "simple not equal" $
        assertExpr "1 != 2" $
          Expr $ EBinop
            ONotEquals
            (Expr $ ELit $ LInt 1)
            (Expr $ ELit $ LInt 2),
      testCase "simple equal" $
        assertExpr "1 == 2" $
          Expr $ EBinop
            OEquals
            (Expr $ ELit $ LInt 1)
            (Expr $ ELit $ LInt 2),
      testCase "simple greater than" $
        assertExpr "1 > 2" $
          Expr $ EBinop
            OGreaterThan
            (Expr $ ELit $ LInt 1)
            (Expr $ ELit $ LInt 2),
      testCase "simple greater than or equal" $
        assertExpr "1 >= 2" $
          Expr $ EBinop
            OGreaterThanEq
            (Expr $ ELit $ LInt 1)
            (Expr $ ELit $ LInt 2),
      testCase "simple less than" $
        assertExpr "1 < 2" $
          Expr $ EBinop
            OLessThan
            (Expr $ ELit $ LInt 1)
            (Expr $ ELit $ LInt 2),
      testCase "simple less than or equal" $
        assertExpr "1 <= 2" $
          Expr $ EBinop
            OLessThanEq
            (Expr $ ELit $ LInt 1)
            (Expr $ ELit $ LInt 2)
  ]

assertProgram :: String -> Program -> Assertion
assertProgram src ast = parseProgram ParserConfig src @?= Right ast

assertExpr :: String -> Expr -> Assertion
assertExpr src ast = parseExpr ParserConfig src @?= Right ast

