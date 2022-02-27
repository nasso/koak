module Test.Koa.Parser.Returns (returnTests) where

import Koa.Syntax
import Test.Koa.Parser.Utils
import Test.Koa.Util
import Test.Tasty
import Test.Tasty.HUnit

valid :: [TestTree]
valid =
  [ testCase "empty" $ assertProgram "" (Program []),
    testCase "empty main" $
      assertProgram
        "fn main(): () { }"
        ( Program
            [ DFn
                (Ident "main")
                []
                TEmpty
                (BExpr [] Nothing)
            ]
        ),
    testCase "main returning empty" $
      assertProgram
        "fn main(): () {\n\
        \  ()\n\
        \}\n"
        ( Program
            [ DFn
                (Ident "main")
                []
                TEmpty
                (BExpr [] $ Just litEmpty)
            ]
        ),
    testCase "main returning zero" $
      assertProgram
        "fn main(): i32 {\n\
        \  0\n\
        \}\n"
        ( Program
            [ DFn
                (Ident "main")
                []
                TInt32
                (BExpr [] $ Just $ litI32 0)
            ]
        ),
    testCase "function returning a binary operator" $
      assertProgram
        "fn test(): i32 {\n\
        \  1 + 2\n\
        \}\n"
        ( Program
            [ DFn
                (Ident "test")
                []
                TInt32
                ( BExpr [] $
                    Just $
                      Expr $
                        EBinop
                          OAdd
                          (litI32 1)
                          (litI32 2)
                )
            ]
        ),
    testCase "function returning a binary operator using a variable" $
      assertProgram
        "fn test(): i32 {\n\
        \  a + b\n\
        \}\n"
        ( Program
            [ DFn
                (Ident "test")
                []
                TInt32
                ( BExpr [] $
                    Just $
                      Expr $
                        EBinop
                          OAdd
                          (varI32 "a")
                          (varI32 "b")
                )
            ]
        ),
    testCase "return statement returning 0" $
      assertProgram "fn f(): i32 { return 0; }\n" $
        Program
          [ DFn
              (Ident "f")
              []
              TInt32
              ( BExpr
                  [SReturn $ litI32 0]
                  Nothing
              )
          ],
    testCase "return statement returning void" $
      assertProgram "fn f(): () { return (); }\n" $
        Program
          [ DFn
              (Ident "f")
              []
              TEmpty
              ( BExpr
                  [SReturn litEmpty]
                  Nothing
              )
          ],
    testCase "return statement returning void twice" $
      assertProgram "fn f(): () { return (); return (); }\n" $
        Program
          [ DFn
              (Ident "f")
              []
              TEmpty
              ( BExpr
                  [ SReturn litEmpty,
                    SReturn litEmpty
                  ]
                  Nothing
              )
          ]
  ]

returnTests :: [TestTree]
returnTests =
  [ testGroup "Valid" valid
  ]
