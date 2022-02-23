module Test.Koa.Analyser.ControlFlow (controlFlowTests) where

import Koa.Analyser
import Koa.Syntax
import Test.Koa.Analyser.Util
import Test.Tasty
import Test.Tasty.HUnit

controlFlowTests :: [TestTree]
controlFlowTests =
  [ testGroup "Valid" validPrograms,
    testGroup "Invalid" invalidPrograms
  ]

validPrograms :: [TestTree]
validPrograms =
  [ testCase "basic if branch" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $
                  Expr
                    ( EIf
                        (litBool True)
                        (litI32 0)
                        (litI32 1)
                    )
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $
                  ExprT
                    ( EIf
                        (litBool True)
                        (litI32 0)
                        (litI32 1),
                      TInt32
                    )
            ]
        ),
    testCase "while loop" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $
                  Expr
                    ( EWhile
                        (litBool True)
                        (BExpr [] litEmpty)
                    )
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $
                  ExprT
                    ( EWhile
                        (litBool True)
                        (BExpr [] litEmpty),
                      TEmpty
                    )
            ]
        )
  ]

invalidPrograms :: [TestTree]
invalidPrograms =
  [ testCase "if branch condition isn't a boolean" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $ Expr $ EIf litEmpty litEmpty litEmpty
            ]
        )
        (ETypeMismatch TBool TEmpty),
    testCase "if branch types mismatch" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $ Expr $ EIf (litBool True) (litI32 0) litEmpty
            ]
        )
        (ETypeMismatch TInt32 TEmpty),
    testCase "while loop condition isn't a boolean" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $ Expr $ EWhile litEmpty $ BExpr [] litEmpty
            ]
        )
        (ETypeMismatch TBool TEmpty)
  ]
