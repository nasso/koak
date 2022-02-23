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
                BExpr [] litEmpty
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr [] litEmpty
            ]
        ),
    testCase "main returning zero" $
      assertValidProgram
        ( Program
            [ DFn (Ident "main") [] TInt32 $
                BExpr [] $ litI32 0
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TInt32 $
                BExpr [] $ litI32 0
            ]
        ),
    testCase "many functions" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ litI32 0,
              DFn (Ident "bar") [] TEmpty $
                BExpr [] litEmpty
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ litI32 0,
              DFn (Ident "bar") [] TEmpty $
                BExpr [] litEmpty
            ]
        ),
    testCase "call function defined before caller" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ litI32 0,
              DFn (Ident "bar") [] TInt32 $
                BExpr [] $ Expr (ECall (Ident "foo") [])
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ litI32 0,
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
                BExpr [] $ litI32 0
            ]
        )
        ( Program
            [ DFn (Ident "bar") [] TInt32 $
                BExpr [] $ ExprT (ECall (Ident "foo") [], TInt32),
              DFn (Ident "foo") [] TInt32 $
                BExpr [] $ litI32 0
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
                  [SExpr $ Expr (ECall (Ident "foo") [])]
                  litEmpty,
              DFn (Ident "foo") [] TInt32 $
                BExpr [] $ litI32 0
            ]
        )
        ( Program
            [ DFn (Ident "bar") [] TEmpty $
                BExpr
                  [SExpr $ ExprT (ECall (Ident "foo") [], TInt32)]
                  litEmpty,
              DFn (Ident "foo") [] TInt32 $
                BExpr [] $ litI32 0
            ]
        )
  ]

invalidPrograms :: [TestTree]
invalidPrograms =
  [ testCase "return type mismatch" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr [] $ litI32 0
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
                BExpr [] $ litI32 0,
              DFn (Ident "bar") [] TEmpty $
                BExpr [] $ Expr (ECall (Ident "foo") [])
            ]
        )
        (ETypeMismatch TEmpty TInt32)
  ]
