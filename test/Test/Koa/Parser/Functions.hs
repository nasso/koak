module Test.Koa.Parser.Functions (functionsTests) where

import Koa.Syntax.HIR
import Test.Koa.Parser.Utils
import Test.Tasty
import Test.Tasty.HUnit

functionsTests :: [TestTree]
functionsTests =
  [ testCase "single argument" $
      assertProgram "fn f(x: i32) { }" $
        Program
          [ DFn
              (Ident "f")
              [TBinding (PIdent $ Ident "x") TInt32]
              TEmpty
              (BExpr [] Nothing)
          ],
    testCase "multiple arguments" $
      assertProgram "fn f(x: i32, y: f64, z: bool) { }" $
        Program
          [ DFn
              (Ident "f")
              [ TBinding (PIdent $ Ident "x") TInt32,
                TBinding (PIdent $ Ident "y") TFloat64,
                TBinding (PIdent $ Ident "z") TBool
              ]
              TEmpty
              (BExpr [] Nothing)
          ],
    testCase "mutable argument" $
      assertProgram "fn f(mut x: i32) { }" $
        Program
          [ DFn
              (Ident "f")
              [TBinding (PMutIdent $ Ident "x") TInt32]
              TEmpty
              (BExpr [] Nothing)
          ],
    testCase "ignored arguments" $
      assertProgram "fn f(_: i32, _: f64) { }" $
        Program
          [ DFn
              (Ident "f")
              [ TBinding PWildcard TInt32,
                TBinding PWildcard TFloat64
              ]
              TEmpty
              (BExpr [] Nothing)
          ],
    testCase "many different patterns" $
      assertProgram "fn f(x: i32, _: f64, mut z: bool, w: ()) { }" $
        Program
          [ DFn
              (Ident "f")
              [ TBinding (PIdent $ Ident "x") TInt32,
                TBinding PWildcard TFloat64,
                TBinding (PMutIdent $ Ident "z") TBool,
                TBinding (PIdent $ Ident "w") TEmpty
              ]
              TEmpty
              (BExpr [] Nothing)
          ]
  ]
