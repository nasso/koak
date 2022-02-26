module Test.Koa.Analyser.Functions (functionsTests) where

import Koa.Analyser
import Koa.Syntax.HIR
import Test.Koa.Analyser.Util
import Test.Koa.Util
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
    testCase "missing final expression" $
      assertValidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr [] Nothing
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr [] Nothing
            ]
        ),
    testCase "function returning empty" $
      assertValidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr [] $ Just litEmpty
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr [] $ Just litEmpty
            ]
        ),
    testCase "function returning zero" $
      assertValidProgram
        ( Program
            [ DFn (Ident "main") [] TInt32 $
                BExpr [] $ Just $ litI32 0
            ]
        )
        ( Program
            [ DFn (Ident "main") [] TInt32 $
                BExpr [] $ Just $ litI32 0
            ]
        ),
    testCase "many functions" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ litI32 0,
              DFn (Ident "bar") [] TEmpty $
                BExpr [] $ Just litEmpty
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ litI32 0,
              DFn (Ident "bar") [] TEmpty $
                BExpr [] $ Just litEmpty
            ]
        ),
    testCase "call function defined before caller" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ litI32 0,
              DFn (Ident "bar") [] TInt32 $
                BExpr [] $ Just $ Expr (ECall (Ident "foo") [])
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ litI32 0,
              DFn (Ident "bar") [] TInt32 $
                BExpr [] $ Just $ ExprT (ECall (Ident "foo") [], TInt32)
            ]
        ),
    testCase "call function defined after caller" $
      assertValidProgram
        ( Program
            [ DFn (Ident "bar") [] TInt32 $
                BExpr [] $ Just $ Expr (ECall (Ident "foo") []),
              DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ litI32 0
            ]
        )
        ( Program
            [ DFn (Ident "bar") [] TInt32 $
                BExpr [] $ Just $ ExprT (ECall (Ident "foo") [], TInt32),
              DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ litI32 0
            ]
        ),
    testCase "self-calling function" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ Expr (ECall (Ident "foo") [])
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ ExprT (ECall (Ident "foo") [], TInt32)
            ]
        ),
    testCase "call statement" $
      assertValidProgram
        ( Program
            [ DFn (Ident "bar") [] TEmpty $
                BExpr
                  [SExpr $ Expr (ECall (Ident "foo") [])]
                  $ Just litEmpty,
              DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ litI32 0
            ]
        )
        ( Program
            [ DFn (Ident "bar") [] TEmpty $
                BExpr
                  [SExpr $ ExprT (ECall (Ident "foo") [], TInt32)]
                  $ Just litEmpty,
              DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ litI32 0
            ]
        ),
    testCase "function taking a parameter" $
      assertValidProgram
        ( Program
            [ DFn
                (Ident "identity")
                [TBinding (PIdent $ Ident "a") TInt32]
                TInt32
                $ BExpr [] $ Just $ Expr (EIdent $ Ident "a")
            ]
        )
        ( Program
            [ DFn
                (Ident "identity")
                [TBinding (PIdent $ Ident "a") TInt32]
                TInt32
                $ BExpr [] $ Just $ ExprT (EIdent $ Ident "a", TInt32)
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
                  $ Just litEmpty
            ]
        )
        ( Program
            [ DFn
                (Ident "foo")
                [TBinding (PMutIdent $ Ident "a") TInt32]
                TEmpty
                $ BExpr
                  [SExpr $ ExprT (EAssign (Ident "a") (litI32 0), TInt32)]
                  $ Just litEmpty
            ]
        ),
    testCase "function call with one argument" $
      assertValidProgram
        ( Program
            [ DFn
                (Ident "identity")
                [TBinding (PIdent $ Ident "a") TInt32]
                TInt32
                $ BExpr [] $ Just $ Expr (EIdent $ Ident "a"),
              DFn (Ident "main") [] TInt32 $
                BExpr [] $ Just $ Expr (ECall (Ident "identity") [litI32 0])
            ]
        )
        ( Program
            [ DFn
                (Ident "identity")
                [TBinding (PIdent $ Ident "a") TInt32]
                TInt32
                $ BExpr [] $ Just $ ExprT (EIdent $ Ident "a", TInt32),
              DFn (Ident "main") [] TInt32 $
                BExpr [] $
                  Just $ ExprT (ECall (Ident "identity") [litI32 0], TInt32)
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
                  Just $
                    Expr
                      ( EBinop
                          OAdd
                          (Expr (EIdent $ Ident "a"))
                          (Expr (EIdent $ Ident "b"))
                      ),
              DFn (Ident "main") [] TInt32 $
                BExpr [] $
                  Just $
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
                  Just $
                    ExprT
                      ( EBinop
                          OAdd
                          (ExprT (EIdent $ Ident "a", TInt32))
                          (ExprT (EIdent $ Ident "b", TInt32)),
                        TInt32
                      ),
              DFn (Ident "main") [] TInt32 $
                BExpr [] $
                  Just $
                    ExprT
                      ( ECall (Ident "add") [litI32 1, litI32 2],
                        TInt32
                      )
            ]
        ),
    testCase "return statement with no last expression" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr
                  [SReturn $ litI32 0]
                  Nothing
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr
                  [SReturn $ litI32 0]
                  Nothing
            ]
        )
  ]

invalidPrograms :: [TestTree]
invalidPrograms =
  [ testCase "return type mismatch" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "main") [] TEmpty $
                BExpr [] $ Just $ litI32 0
            ]
        )
        (ETypeMismatch TEmpty TInt32),
    testCase "empty body when return type isn't ()" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "main") [] TInt32 $
                BExpr [] Nothing
            ]
        )
        (ETypeMismatch TInt32 TEmpty),
    testCase "call undefined function" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "bar") [] TEmpty $
                BExpr [] $ Just $ Expr (ECall (Ident "foo") [])
            ]
        )
        (EUndefinedSymbol $ Ident "foo"),
    testCase "function call return type mismatch" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ litI32 0,
              DFn (Ident "bar") [] TEmpty $
                BExpr [] $ Just $ Expr (ECall (Ident "foo") [])
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
                $ BExpr [] $ Just $ Expr (EIdent $ Ident "a"),
              DFn (Ident "main") [] TInt32 $
                BExpr [] $ Just $ Expr (ECall (Ident "identity") [])
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
                $ BExpr [] $ Just $ Expr (EIdent $ Ident "a"),
              DFn (Ident "main") [] TInt32 $
                BExpr [] $
                  Just $ Expr (ECall (Ident "identity") [litI32 0, litI32 1])
            ]
        )
        EInvalidArguments,
    testCase "function call with arguments of the wrong type" $
      assertInvalidProgram
        ( Program
            [ DFn
                (Ident "identity")
                [TBinding (PIdent $ Ident "a") TInt32]
                TInt32
                $ BExpr [] $ Just $ Expr (EIdent $ Ident "a"),
              DFn (Ident "main") [] TInt32 $
                BExpr [] $ Just $ Expr (ECall (Ident "identity") [litBool True])
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
                  $ Just litEmpty
            ]
        )
        (EMutationOfImmutable $ Ident "a"),
    testCase "return statement with wrong type" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr
                  [SReturn $ litI32 0]
                  Nothing
            ]
        )
        (ETypeMismatch TEmpty TInt32)
  ]
