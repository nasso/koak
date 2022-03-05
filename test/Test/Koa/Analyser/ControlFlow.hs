module Test.Koa.Analyser.ControlFlow (controlFlowTests) where

import Koa.Analyser
import Koa.Syntax.HIR
import Test.Koa.Analyser.Util
import Test.Koa.Util
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
                  Just $
                    Expr
                      ( EIf
                          (litBool True)
                          (BExpr [] $ Just $ litI32 0)
                          (BExpr [] $ Just $ litI32 1)
                      )
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $
                  Just $
                    ExprT
                      ( EIf
                          (litBool True)
                          (BExpr [] $ Just $ litI32 0)
                          (BExpr [] $ Just $ litI32 1),
                        TInt32
                      )
            ]
        ),
    testCase "while loop" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $
                  Just $
                    Expr
                      ( EWhile
                          (litBool True)
                          (BExpr [] $ Just litEmpty)
                      )
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $
                  Just $
                    ExprT
                      ( EWhile
                          (litBool True)
                          (BExpr [] $ Just litEmpty),
                        TEmpty
                      )
            ]
        ),
    testCase "for loop" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $
                  Just $
                    Expr
                      ( EFor
                          (SLet (PMutIdent $ Ident "i") Nothing (litI32 0))
                          ( Expr
                              (EBinop OLessThan (varI32 "i") (litI32 10))
                          )
                          ( Expr
                              ( EAssign
                                  (Ident "i")
                                  ( Expr
                                      (EBinop OAdd (varI32 "i") (litI32 1))
                                  )
                              )
                          )
                          (BExpr [SExpr $ varI32 "i"] Nothing)
                      )
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $
                  Just $
                    ExprT
                      ( EFor
                          ( SLet
                              (PMutIdent $ Ident "i")
                              (Just TInt32)
                              (litI32 0)
                          )
                          ( ExprT
                              ( EBinop OLessThan (varI32 "i") (litI32 10),
                                TBool
                              )
                          )
                          ( ExprT
                              ( EAssign
                                  (Ident "i")
                                  ( ExprT
                                      ( EBinop OAdd (varI32 "i") (litI32 1),
                                        TInt32
                                      )
                                  ),
                                TInt32
                              )
                          )
                          (BExpr [SExpr $ varI32 "i"] Nothing),
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
                  Just $
                    Expr $
                      EIf
                        litEmpty
                        (BExpr [] $ Just litEmpty)
                        (BExpr [] $ Just litEmpty)
            ]
        )
        (ETypeMismatch TBool TEmpty),
    testCase "if branch types mismatch" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $
                  Just $
                    Expr $
                      EIf
                        (litBool True)
                        (BExpr [] $ Just $ litI32 0)
                        (BExpr [] $ Just litEmpty)
            ]
        )
        (ETypeMismatch TInt32 TEmpty),
    testCase "while loop condition isn't a boolean" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $
                  Just $ Expr $ EWhile litEmpty $ BExpr [] $ Just litEmpty
            ]
        )
        (ETypeMismatch TBool TEmpty),
    testCase "while loop body doesn't evaluate to empty" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr
                  [ SExpr $
                      Expr $
                        EWhile (litBool False) $
                          BExpr [] $
                            Just $ litI32 4
                  ]
                  Nothing
            ]
        )
        (ETypeMismatch TEmpty TInt32),
    testCase "for loop condition isn't a boolean" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $
                  Just $
                    Expr $
                      EFor
                        (SExpr litEmpty)
                        litEmpty
                        litEmpty
                        (BExpr [] $ Just litEmpty)
            ]
        )
        (ETypeMismatch TBool TEmpty),
    testCase "for loop body doesn't evaluate to empty" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr
                  [ SExpr $
                      Expr $
                        EFor
                          (SExpr litEmpty)
                          (litBool True)
                          litEmpty
                          (BExpr [] $ Just $ litI32 4)
                  ]
                  Nothing
            ]
        )
        (ETypeMismatch TEmpty TInt32)
  ]
