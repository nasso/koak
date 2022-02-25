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
                        (BExpr [] $ litI32 0)
                        (BExpr [] $ litI32 1)
                    )
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $
                  ExprT
                    ( EIf
                        (litBool True)
                        (BExpr [] $ litI32 0)
                        (BExpr [] $ litI32 1),
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
        ),
    testCase "for loop" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $
                  Expr
                    ( EFor
                        (SLet (PMutIdent $ Ident "i") Nothing (litI32 0))
                        ( Expr
                            ( EBinop
                                OLessThan
                                (Expr (EIdent $ Ident "i"))
                                (litI32 10)
                            )
                        )
                        ( Expr
                            ( EAssign
                                (Ident "i")
                                ( Expr
                                    ( EBinop
                                        OAdd
                                        (Expr (EIdent $ Ident "i"))
                                        (litI32 1)
                                    )
                                )
                            )
                        )
                        (BExpr [] $ Expr (EIdent $ Ident "i"))
                    )
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $
                  ExprT
                    ( EFor
                        (SLet (PMutIdent $ Ident "i") (Just TInt32) (litI32 0))
                        ( ExprT
                            ( EBinop
                                OLessThan
                                (ExprT (EIdent $ Ident "i", TInt32))
                                (litI32 10),
                              TBool
                            )
                        )
                        ( ExprT
                            ( EAssign
                                (Ident "i")
                                ( ExprT
                                    ( EBinop
                                        OAdd
                                        (ExprT (EIdent $ Ident "i", TInt32))
                                        (litI32 1),
                                      TInt32
                                    )
                                ),
                              TInt32
                            )
                        )
                        (BExpr [] $ ExprT (EIdent $ Ident "i", TInt32)),
                      TInt32
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
                  Expr $
                    EIf
                      litEmpty
                      (BExpr [] litEmpty)
                      (BExpr [] litEmpty)
            ]
        )
        (ETypeMismatch TBool TEmpty),
    testCase "if branch types mismatch" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $
                  Expr $
                    EIf
                      (litBool True)
                      (BExpr [] $ litI32 0)
                      (BExpr [] litEmpty)
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
        (ETypeMismatch TBool TEmpty),
    testCase "for loop condition isn't a boolean" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $
                  Expr $
                    EFor
                      (SExpr litEmpty)
                      litEmpty
                      litEmpty
                      (BExpr [] litEmpty)
            ]
        )
        (ETypeMismatch TBool TEmpty)
  ]
