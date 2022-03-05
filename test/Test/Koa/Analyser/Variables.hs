module Test.Koa.Analyser.Variables (variablesTests) where

import Koa.Analyser
import Koa.Syntax.HIR
import Test.Koa.Analyser.Util
import Test.Koa.Util
import Test.Tasty
import Test.Tasty.HUnit

variablesTests :: [TestTree]
variablesTests =
  [ testGroup "Valid" validPrograms,
    testGroup "Invalid" invalidPrograms
  ]

validPrograms :: [TestTree]
validPrograms =
  [ testCase "define immutable variable" $
      assertValidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet
                      (PIdent $ Ident "foo")
                      Nothing
                      (litI32 4)
                  ]
                  $ Just litEmpty
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet
                      (PIdent $ Ident "foo")
                      (Just TInt32)
                      (litI32 4)
                  ]
                  $ Just litEmpty
            ]
        ),
    testCase "define mutable variable" $
      assertValidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet
                      (PMutIdent $ Ident "foo")
                      Nothing
                      (litI32 4)
                  ]
                  $ Just litEmpty
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet
                      (PMutIdent $ Ident "foo")
                      (Just TInt32)
                      (litI32 4)
                  ]
                  $ Just litEmpty
            ]
        ),
    testCase "define immutable variable with explicit typing" $
      assertValidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet
                      (PIdent $ Ident "foo")
                      (Just TInt32)
                      (litI32 4)
                  ]
                  $ Just litEmpty
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet
                      (PIdent $ Ident "foo")
                      (Just TInt32)
                      (litI32 4)
                  ]
                  $ Just litEmpty
            ]
        ),
    testCase "define mutable variable with explicit typing" $
      assertValidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet
                      (PMutIdent $ Ident "foo")
                      (Just TInt32)
                      (litI32 4)
                  ]
                  $ Just litEmpty
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet
                      (PMutIdent $ Ident "foo")
                      (Just TInt32)
                      (litI32 4)
                  ]
                  $ Just litEmpty
            ]
        ),
    testCase "variable reference" $
      assertValidProgram
        ( Program
            [ DFn (Ident "main") [] TInt32 $
                BExpr
                  [ SLet (PIdent $ Ident "foo") Nothing (litI32 4)
                  ]
                  $ Just $ Expr (EIdent $ Ident "foo")
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TInt32 $
                BExpr
                  [ SLet (PIdent $ Ident "foo") (Just TInt32) (litI32 4)
                  ]
                  $ Just $ ExprT (EIdent $ Ident "foo", TInt32)
            ]
        ),
    testCase "copy variable into another" $
      assertValidProgram
        ( Program
            [ DFn (Ident "main") [] TInt32 $
                BExpr
                  [ SLet (PIdent $ Ident "foo") Nothing (litI32 4),
                    SLet
                      (PIdent $ Ident "bar")
                      Nothing
                      (Expr (EIdent $ Ident "foo"))
                  ]
                  $ Just $ Expr (EIdent $ Ident "bar")
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TInt32 $
                BExpr
                  [ SLet (PIdent $ Ident "foo") (Just TInt32) (litI32 4),
                    SLet
                      (PIdent $ Ident "bar")
                      (Just TInt32)
                      (ExprT (EIdent $ Ident "foo", TInt32))
                  ]
                  $ Just $ ExprT (EIdent $ Ident "bar", TInt32)
            ]
        ),
    testCase "re-assign mutable variable" $
      assertValidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet (PMutIdent $ Ident "foo") Nothing (litI32 4),
                    SExpr $ Expr $ EAssign (Ident "foo") $ litI32 5
                  ]
                  $ Just litEmpty
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet (PMutIdent $ Ident "foo") (Just TInt32) (litI32 4),
                    SExpr $ ExprT (EAssign (Ident "foo") $ litI32 5, TInt32)
                  ]
                  $ Just litEmpty
            ]
        ),
    testCase "refer to variable defined in parent scope" $
      assertValidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet (PIdent $ Ident "foo") Nothing (litI32 4),
                    SExpr . Expr . EBlock $
                      BExpr
                        [ SLet
                            (PIdent $ Ident "bar")
                            Nothing
                            (Expr $ EIdent $ Ident "foo")
                        ]
                        $ Just litEmpty
                  ]
                  $ Just litEmpty
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet (PIdent $ Ident "foo") (Just TInt32) (litI32 4),
                    SExpr $
                      ExprT
                        ( EBlock $
                            BExpr
                              [ SLet
                                  (PIdent $ Ident "bar")
                                  (Just TInt32)
                                  (ExprT (EIdent $ Ident "foo", TInt32))
                              ]
                              $ Just litEmpty,
                          TEmpty
                        )
                  ]
                  $ Just litEmpty
            ]
        )
  ]

invalidPrograms :: [TestTree]
invalidPrograms =
  [ testCase "define immutable variable with invalid explicit typing" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet
                      (PIdent $ Ident "foo")
                      (Just TBool)
                      (litI32 1)
                  ]
                  $ Just litEmpty
            ]
        )
        (ETypeMismatch TBool TInt32),
    testCase "define mutable variable with invalid explicit typing" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet
                      (PMutIdent $ Ident "foo")
                      (Just TBool)
                      (litI32 1)
                  ]
                  $ Just litEmpty
            ]
        )
        (ETypeMismatch TBool TInt32),
    testCase "refer to undefined variable" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr [] $ Just $ Expr $ EIdent $ Ident "foo"
            ]
        )
        (EUndefinedSymbol $ Ident "foo"),
    testCase "variable has wrong type" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet (PIdent $ Ident "foo") Nothing (litI32 1)
                  ]
                  $ Just (Expr $ EIdent $ Ident "foo")
            ]
        )
        (ETypeMismatch TEmpty TInt32),
    testCase "assign to undefined variable" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SExpr $ Expr $ EAssign (Ident "foo") $ litI32 4
                  ]
                  $ Just litEmpty
            ]
        )
        (EUndefinedSymbol $ Ident "foo"),
    testCase "re-assign immutable variable" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet (PIdent $ Ident "foo") Nothing (litI32 4),
                    SExpr $ Expr $ EAssign (Ident "foo") $ litI32 5
                  ]
                  $ Just litEmpty
            ]
        )
        (EMutationOfImmutable $ Ident "foo"),
    testCase "re-assign with wrong type" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet (PMutIdent $ Ident "foo") Nothing (litI32 4),
                    SExpr $ Expr $ EAssign (Ident "foo") $ litBool True
                  ]
                  $ Just litEmpty
            ]
        )
        (ETypeMismatch TInt32 TBool),
    testCase "call non-function variable" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet (PIdent $ Ident "foo") Nothing (litI32 4),
                    SExpr $ Expr $ ECall (Ident "foo") []
                  ]
                  $ Just litEmpty
            ]
        )
        (ENotAFunction $ Ident "foo"),
    testCase "refer to out-of-scope variable" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "main") [] TInt32 $
                BExpr
                  [ SExpr . Expr . EBlock $
                      BExpr
                        [ SLet (PIdent $ Ident "foo") Nothing (litI32 4)
                        ]
                        $ Just litEmpty
                  ]
                  $ Just (Expr $ EIdent $ Ident "foo")
            ]
        )
        (EUndefinedSymbol $ Ident "foo")
  ]
