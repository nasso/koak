module Test.Koa.Parser.Let (letTests) where

import Koa.Syntax
import Test.Koa.Parser.Utils
import Test.Koa.Util
import Test.Tasty
import Test.Tasty.HUnit

valid :: [TestTree]
valid =
  [ testCase "non mutable ident a" $
      assertProgram
        "fn f(): () { let a = 1; }"
        ( Program
            [ DFn
                (Ident "f")
                []
                TEmpty
                ( BExpr
                    [ SLet
                        (PIdent $ Ident "a")
                        Nothing
                        (litI32 1)
                    ]
                    Nothing
                )
            ]
        ),
    testCase "mutable ident a" $
      assertProgram
        "fn f(): () { let mut a = 1; }"
        ( Program
            [ DFn
                (Ident "f")
                []
                TEmpty
                ( BExpr
                    [ SLet
                        (PMutIdent $ Ident "a")
                        Nothing
                        (litI32 1)
                    ]
                    Nothing
                )
            ]
        ),
    testCase "non mutable wildcard" $
      assertProgram
        "fn f(): () { let _ = 1; }"
        ( Program
            [ DFn
                (Ident "f")
                []
                TEmpty
                ( BExpr
                    [ SLet
                        PWildcard
                        Nothing
                        (litI32 1)
                    ]
                    Nothing
                )
            ]
        ),
    testCase "typed non mutable ident a" $
      assertProgram
        "fn f(): () { let a: i32 = 1; }"
        ( Program
            [ DFn
                (Ident "f")
                []
                TEmpty
                ( BExpr
                    [ SLet
                        (PIdent $ Ident "a")
                        (Just TInt32)
                        (litI32 1)
                    ]
                    Nothing
                )
            ]
        ),
    testCase "typed mutable ident a" $
      assertProgram
        "fn f(): () { let mut a: i32 = 1; }"
        ( Program
            [ DFn
                (Ident "f")
                []
                TEmpty
                ( BExpr
                    [ SLet
                        (PMutIdent $ Ident "a")
                        (Just TInt32)
                        (litI32 1)
                    ]
                    Nothing
                )
            ]
        ),
    testCase "typed non mutable wildcard" $
      assertProgram
        "fn f(): () { let _: i32 = 1; }"
        ( Program
            [ DFn
                (Ident "f")
                []
                TEmpty
                ( BExpr
                    [ SLet
                        PWildcard
                        (Just TInt32)
                        (litI32 1)
                    ]
                    Nothing
                )
            ]
        ),
    testCase "mutable wildcard" $
      assertError
        "fn f(): () { let mut _ = 1; }"
  ]

letTests :: [TestTree]
letTests = [testGroup "Valid" valid]
