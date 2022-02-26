module Test.Koa.Parser.Loop (loopTests) where

import Koa.Syntax.HIR
import Test.Koa.Parser.Utils
import Test.Koa.Util
import Test.Tasty
import Test.Tasty.HUnit

validFor :: [TestTree]
validFor =
  [ testCase "for expression" $
      assertExpr "for let a = 5, a <= b, a = a + 1 {}" $
        Expr $
          EFor
            ( SLet
                (PIdent $ Ident "a")
                Nothing
                (litI32 5)
            )
            ( Expr $
                EBinop
                  OLessThanEq
                  (varI32 "a")
                  (varI32 "b")
            )
            ( Expr $
                EAssign
                  (Ident "a")
                  ( Expr $
                      EBinop
                        OAdd
                        (varI32 "a")
                        (litI32 1)
                  )
            )
            (BExpr [] Nothing)
  ]

validWhile :: [TestTree]
validWhile =
  [ testCase "while expression" $
      assertExpr "while a <= b {}" $
        Expr $
          EWhile
            ( Expr $
                EBinop
                  OLessThanEq
                  (varI32 "a")
                  (varI32 "b")
            )
            (BExpr [] Nothing)
  ]

validIf :: [TestTree]
validIf =
  [ testCase "if expression without else" $
      assertExpr "if a <= b {}" $
        Expr $
          EIf
            ( Expr $
                EBinop
                  OLessThanEq
                  (varI32 "a")
                  (varI32 "b")
            )
            (BExpr [] Nothing)
            (BExpr [] Nothing),
    testCase "if expression with else" $
      assertExpr "if a <= b {} else {}" $
        Expr $
          EIf
            ( Expr $
                EBinop
                  OLessThanEq
                  (varI32 "a")
                  (varI32 "b")
            )
            (BExpr [] Nothing)
            (BExpr [] Nothing)
  ]

forTest :: [TestTree]
forTest = [testGroup "Valid" validFor]

ifTest :: [TestTree]
ifTest = [testGroup "Valid" validIf]

whileTest :: [TestTree]
whileTest = [testGroup "Valid" validWhile]

loopTests :: [TestTree]
loopTests =
  [ testGroup "For" forTest,
    testGroup "If" ifTest,
    testGroup "While" whileTest
  ]
