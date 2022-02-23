module Test.Koa.Analyser.Variables (variablesTests) where

import Koa.Analyser
import Koa.Syntax
import Test.Koa.Analyser.Util
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
                  litEmpty
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
                  litEmpty
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
                  litEmpty
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
                  litEmpty
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
                  litEmpty
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
                  litEmpty
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
                  litEmpty
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
                  litEmpty
            ]
        ),
    testCase "variable reference" $
      assertValidProgram
        ( Program
            [ DFn (Ident "main") [] TInt32 $
                BExpr
                  [ SLet (PIdent $ Ident "foo") Nothing (litI32 4)
                  ]
                  $ Expr (EIdent $ Ident "foo")
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TInt32 $
                BExpr
                  [ SLet (PIdent $ Ident "foo") (Just TInt32) (litI32 4)
                  ]
                  $ ExprT (EIdent $ Ident "foo", TInt32)
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
                  $ Expr (EIdent $ Ident "bar")
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
                  $ ExprT (EIdent $ Ident "bar", TInt32)
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
                  litEmpty
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet (PMutIdent $ Ident "foo") (Just TInt32) (litI32 4),
                    SExpr $ ExprT (EAssign (Ident "foo") $ litI32 5, TInt32)
                  ]
                  litEmpty
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
                        litEmpty
                  ]
                  litEmpty
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
                              litEmpty,
                          TEmpty
                        )
                  ]
                  litEmpty
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
                  litEmpty
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
                  litEmpty
            ]
        )
        (ETypeMismatch TBool TInt32),
    testCase "refer to undefined variable" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr [] (Expr $ EIdent $ Ident "foo")
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
                  (Expr $ EIdent $ Ident "foo")
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
                  litEmpty
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
                  litEmpty
            ]
        )
        (EMutationOfImmutable $ Ident "foo"),
    testCase "re-assign with wrong type" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr
                  [ SLet (PIdent $ Ident "foo") Nothing (litI32 4),
                    SExpr $ Expr $ EAssign (Ident "foo") $ litBool True
                  ]
                  litEmpty
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
                  litEmpty
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
                        litEmpty
                  ]
                  (Expr $ EIdent $ Ident "foo")
            ]
        )
        (EUndefinedSymbol $ Ident "foo")
  ]
