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
    testCase "function returning empty" $
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
    testCase "function returning zero" $
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
        ),
    testCase "function taking a parameter" $
      assertValidProgram
        ( Program
            [ DFn
                (Ident "identity")
                [TBinding (PIdent $ Ident "a") TInt32]
                TInt32
                $ BExpr [] $ Expr (EIdent $ Ident "a")
            ]
        )
        ( Program
            [ DFn
                (Ident "identity")
                [TBinding (PIdent $ Ident "a") TInt32]
                TInt32
                $ BExpr [] $ ExprT (EIdent $ Ident "a", TInt32)
            ]
        ),
    testCase "mutable parameter" $
      assertValidProgram
        ( Program
            [ DFn
                (Ident "foo")
                [TBinding (PMutIdent $ Ident "a") TInt32]
                TEmpty
                $ BExpr
                  [SExpr $ Expr (EAssign (Ident "a") (litI32 0))]
                  litEmpty
            ]
        )
        ( Program
            [ DFn
                (Ident "foo")
                [TBinding (PMutIdent $ Ident "a") TInt32]
                TEmpty
                $ BExpr
                  [SExpr $ ExprT (EAssign (Ident "a") (litI32 0), TInt32)]
                  litEmpty
            ]
        ),
    testCase "function call with one argument" $
      assertValidProgram
        ( Program
            [ DFn
                (Ident "identity")
                [TBinding (PIdent $ Ident "a") TInt32]
                TInt32
                $ BExpr [] $ Expr (EIdent $ Ident "a"),
              DFn (Ident "main") [] TInt32 $
                BExpr [] $ Expr (ECall (Ident "identity") [litI32 0])
            ]
        )
        ( Program
            [ DFn
                (Ident "identity")
                [TBinding (PIdent $ Ident "a") TInt32]
                TInt32
                $ BExpr [] $ ExprT (EIdent $ Ident "a", TInt32),
              DFn (Ident "main") [] TInt32 $
                BExpr [] $ ExprT (ECall (Ident "identity") [litI32 0], TInt32)
            ]
        ),
    testCase "add function" $
      assertValidProgram
        ( Program
            [ DFn
                (Ident "add")
                [ TBinding (PIdent $ Ident "a") TInt32,
                  TBinding (PIdent $ Ident "b") TInt32
                ]
                TInt32
                $ BExpr [] $
                  Expr
                    ( EBinop
                        OAdd
                        (Expr (EIdent $ Ident "a"))
                        (Expr (EIdent $ Ident "b"))
                    ),
              DFn (Ident "main") [] TInt32 $
                BExpr [] $
                  Expr
                    ( ECall (Ident "add") [litI32 1, litI32 2]
                    )
            ]
        )
        ( Program
            [ DFn
                (Ident "add")
                [ TBinding (PIdent $ Ident "a") TInt32,
                  TBinding (PIdent $ Ident "b") TInt32
                ]
                TInt32
                $ BExpr [] $
                  ExprT
                    ( EBinop
                        OAdd
                        (ExprT (EIdent $ Ident "a", TInt32))
                        (ExprT (EIdent $ Ident "b", TInt32)),
                      TInt32
                    ),
              DFn (Ident "main") [] TInt32 $
                BExpr [] $
                  ExprT
                    ( ECall (Ident "add") [litI32 1, litI32 2],
                      TInt32
                    )
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
        (ETypeMismatch TEmpty TInt32),
    testCase "function call with too few arguments" $
      assertInvalidProgram
        ( Program
            [ DFn
                (Ident "identity")
                [TBinding (PIdent $ Ident "a") TInt32]
                TInt32
                $ BExpr [] $ Expr (EIdent $ Ident "a"),
              DFn (Ident "main") [] TInt32 $
                BExpr [] $ Expr (ECall (Ident "identity") [])
            ]
        )
        EInvalidArguments,
    testCase "function call with too many arguments" $
      assertInvalidProgram
        ( Program
            [ DFn
                (Ident "identity")
                [TBinding (PIdent $ Ident "a") TInt32]
                TInt32
                $ BExpr [] $ Expr (EIdent $ Ident "a"),
              DFn (Ident "main") [] TInt32 $
                BExpr [] $ Expr (ECall (Ident "identity") [litI32 0, litI32 1])
            ]
        )
        EInvalidArguments,
    testCase "funciton call with arguments of the wrong type" $
      assertInvalidProgram
        ( Program
            [ DFn
                (Ident "identity")
                [TBinding (PIdent $ Ident "a") TInt32]
                TInt32
                $ BExpr [] $ Expr (EIdent $ Ident "a"),
              DFn (Ident "main") [] TInt32 $
                BExpr [] $ Expr (ECall (Ident "identity") [litBool True])
            ]
        )
        EInvalidArguments,
    testCase "assignment to immutable parameter" $
      assertInvalidProgram
        ( Program
            [ DFn
                (Ident "foo")
                [TBinding (PIdent $ Ident "a") TInt32]
                TEmpty
                $ BExpr
                  [SExpr $ Expr (EAssign (Ident "a") (litI32 0))]
                  litEmpty
            ]
        )
        (EMutationOfImmutable $ Ident "a")
  ]
