module Test.Koa.Parser.Assignment (assignmentTests) where

import Koa.Syntax.HIR
import Test.Koa.Parser.Utils
import Test.Koa.Util
import Test.Tasty
import Test.Tasty.HUnit

valid :: [TestTree]
valid =
  [ testCase "typed non mutable wildcard block empty" $
      assertProgram
        "fn f(): () { a = { }; }"
        ( Program
            [ DFn
                (Ident "f")
                []
                TEmpty
                ( BExpr
                    [ SExpr $
                        Expr
                          ( EAssign
                              (Ident "a")
                              ( Expr $
                                  EBlock $
                                    BExpr [] Nothing
                              )
                          )
                    ]
                    Nothing
                )
            ]
        ),
    testCase "typed non mutable wildcard block" $
      assertProgram
        "fn f(): () { a = { 5 }; }"
        ( Program
            [ DFn
                (Ident "f")
                []
                TEmpty
                ( BExpr
                    [ SExpr $
                        Expr
                          ( EAssign
                              (Ident "a")
                              ( Expr $
                                  EBlock $
                                    BExpr [] (Just $ litI32 5)
                              )
                          )
                    ]
                    Nothing
                )
            ]
        )
  ]

invalid :: [TestTree]
invalid = []

assignmentTests :: [TestTree]
assignmentTests =
  [ testGroup "Valid" valid,
    testGroup "Invalid" invalid
  ]
