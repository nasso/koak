module Test.Koa.Analyser.Functions (functionsTests) where

import Koa.Analyser
import Koa.Syntax
import Test.Koa.Analyser.Util
import Test.Tasty
import Test.Tasty.HUnit

functionsTests :: [TestTree]
functionsTests =
  [ testGroup "Valid" validPrograms,
    testGroup "Invalid" invalidPrograms
  ]

validPrograms :: [TestTree]
validPrograms =
  [ testCase "empty body" $ assertValidProgram (Program []) (Program []),
    testCase "main returning empty" $
      assertValidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr [] $ Expr (ELit LEmpty)
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr [] $ ExprT (ELit LEmpty, TEmpty)
            ]
        ),
    testCase "main returning zero" $
      assertValidProgram
        ( Program
            [ DFn (Ident "main") [] TInt32 $
                BExpr [] $ Expr (ELit $ LInt 0)
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TInt32 $
                BExpr [] $ ExprT (ELit $ LInt 0, TInt32)
            ]
        ),
    testCase "many functions" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Expr (ELit $ LInt 0),
              DFn (Ident "bar") [] TEmpty $
                BExpr [] $ Expr (ELit LEmpty)
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ ExprT (ELit $ LInt 0, TInt32),
              DFn (Ident "bar") [] TEmpty $
                BExpr [] $ ExprT (ELit LEmpty, TEmpty)
            ]
        ),
    testCase "call function defined before caller" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Expr (ELit $ LInt 0),
              DFn (Ident "bar") [] TInt32 $
                BExpr [] $ Expr (ECall (Ident "foo") [])
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ ExprT (ELit $ LInt 0, TInt32),
              DFn (Ident "bar") [] TInt32 $
                BExpr [] $ ExprT (ECall (Ident "foo") [], TInt32)
            ]
        ),
    testCase "call function defined after caller" $
      assertValidProgram
        ( Program
            [ DFn (Ident "bar") [] TInt32 $
                BExpr [] $ Expr (ECall (Ident "foo") []),
              DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Expr (ELit $ LInt 0)
            ]
        )
        ( Program
            [ DFn (Ident "bar") [] TInt32 $
                BExpr [] $ ExprT (ECall (Ident "foo") [], TInt32),
              DFn (Ident "foo") [] TInt32 $
                BExpr [] $ ExprT (ELit $ LInt 0, TInt32)
            ]
        ),
    testCase "self-calling function" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Expr (ECall (Ident "foo") [])
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ ExprT (ECall (Ident "foo") [], TInt32)
            ]
        ),
    testCase "call statement" $
      assertValidProgram
        ( Program
            [ DFn (Ident "bar") [] TEmpty $
                BExpr
                  [ SExpr $ Expr (ECall (Ident "foo") [])
                  ]
                  $ Expr (ELit LEmpty),
              DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Expr (ELit $ LInt 0)
            ]
        )
        ( Program
            [ DFn (Ident "bar") [] TEmpty $
                BExpr
                  [ SExpr $ ExprT (ECall (Ident "foo") [], TInt32)
                  ]
                  $ ExprT (ELit LEmpty, TEmpty),
              DFn (Ident "foo") [] TInt32 $
                BExpr [] $ ExprT (ELit $ LInt 0, TInt32)
            ]
        )
  ]

invalidPrograms :: [TestTree]
invalidPrograms =
  [ testCase "return type mismatch" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr [] $ Expr (ELit $ LInt 0)
            ]
        )
        (ETypeMismatch TEmpty TInt32),
    testCase "call undefined function" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "bar") [] TEmpty $
                BExpr [] $ Expr (ECall (Ident "foo") [])
            ]
        )
        (EUndefinedSymbol $ Ident "foo"),
    testCase "function call return type mismatch" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Expr (ELit $ LInt 0),
              DFn (Ident "bar") [] TEmpty $
                BExpr [] $ Expr (ECall (Ident "foo") [])
            ]
        )
        (ETypeMismatch TEmpty TInt32)
  ]
