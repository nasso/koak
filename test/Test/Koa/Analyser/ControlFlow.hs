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
                        (Expr $ ELit $ LBool True)
                        (Expr $ ELit $ LInt 0)
                        (Expr $ ELit $ LInt 1)
                    )
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $
                  ExprT
                    ( EIf
                        (ExprT (ELit $ LBool True, TBool))
                        (ExprT (ELit $ LInt 0, TInt32))
                        (ExprT (ELit $ LInt 1, TInt32)),
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
                        (Expr $ ELit $ LBool True)
                        (BExpr [] $ Expr $ ELit LEmpty)
                    )
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $
                  ExprT
                    ( EWhile
                        (ExprT (ELit $ LBool True, TBool))
                        (BExpr [] $ ExprT (ELit LEmpty, TEmpty)),
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
                BExpr [] $
                  Expr
                    ( EIf
                        (Expr $ ELit LEmpty)
                        (Expr $ ELit LEmpty)
                        (Expr $ ELit LEmpty)
                    )
            ]
        )
        (ETypeMismatch TBool TEmpty),
    testCase "if branch types mismatch" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $
                  Expr
                    ( EIf
                        (Expr $ ELit $ LBool True)
                        (Expr $ ELit $ LInt 0)
                        (Expr $ ELit LEmpty)
                    )
            ]
        )
        (ETypeMismatch TInt32 TEmpty),
    testCase "while loop condition isn't a boolean" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $
                  Expr
                    ( EWhile
                        (Expr $ ELit LEmpty)
                        (BExpr [] $ Expr $ ELit LEmpty)
                    )
            ]
        )
        (ETypeMismatch TBool TEmpty)
  ]
