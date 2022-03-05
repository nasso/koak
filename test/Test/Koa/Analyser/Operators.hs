module Test.Koa.Analyser.Operators (operatorsTests) where

import Koa.Analyser
import Koa.Syntax.HIR
import Test.Koa.Analyser.Util
import Test.Koa.Util
import Test.Tasty
import Test.Tasty.HUnit

operatorsTests :: [TestTree]
operatorsTests =
  [ testGroup "Valid" validPrograms,
    testGroup "Invalid" invalidPrograms
  ]

validPrograms :: [TestTree]
validPrograms =
  [ testCase "can add i32 and i32" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ Expr (EBinop OAdd (litI32 1) (litI32 2))
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $
                  Just $
                    ExprT
                      ( EBinop OAdd (litI32 1) (litI32 2),
                        TInt32
                      )
            ]
        ),
    testCase "i32 == i32 is a boolean" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TBool $
                BExpr [] $ Just $ Expr (EBinop OEquals (litI32 1) (litI32 2))
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TBool $
                BExpr [] $
                  Just $
                    ExprT
                      ( EBinop OEquals (litI32 1) (litI32 2),
                        TBool
                      )
            ]
        ),
    testCase "unary minus on i32" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ Expr (EUnop ONeg $ litI32 1)
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ ExprT (EUnop ONeg $ litI32 1, TInt32)
            ]
        ),
    testCase "unary minus on f64" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TFloat64 $
                BExpr [] $ Just $ Expr (EUnop ONeg $ litF64 1)
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TFloat64 $
                BExpr [] $ Just $ ExprT (EUnop ONeg $ litF64 1, TFloat64)
            ]
        ),
    testCase "negate boolean" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TBool $
                BExpr [] $ Just $ Expr (EUnop ONot $ litBool False)
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TBool $
                BExpr [] $ Just $ ExprT (EUnop ONot $ litBool False, TBool)
            ]
        )
  ]

invalidPrograms :: [TestTree]
invalidPrograms =
  [ testCase "can't add together i32 and f64" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ Expr (EBinop OAdd (litI32 1) (litF64 2.0))
            ]
        )
        (EInvalidBinop OAdd TInt32 TFloat64),
    testCase "can't add empties together" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $ Just $ Expr (EBinop OAdd litEmpty litEmpty)
            ]
        )
        (EInvalidBinop OAdd TEmpty TEmpty),
    testCase "can't use < on empties" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TBool $
                BExpr [] $ Just $ Expr (EBinop OLessThan litEmpty litEmpty)
            ]
        )
        (EInvalidBinop OLessThan TEmpty TEmpty),
    testCase "can't use < on booleans" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TBool $
                BExpr [] $
                  Just $ Expr (EBinop OLessThan (litBool True) (litBool True))
            ]
        )
        (EInvalidBinop OLessThan TBool TBool),
    testCase "unary minus on boolean" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TBool $
                BExpr [] $ Just $ Expr (EUnop ONeg $ litBool False)
            ]
        )
        (EInvalidUnop ONeg TBool),
    testCase "unary minus on empty" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $ Just $ Expr (EUnop ONeg litEmpty)
            ]
        )
        (EInvalidUnop ONeg TEmpty),
    testCase "boolean not on empty" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $ Just $ Expr (EUnop ONot litEmpty)
            ]
        )
        (EInvalidUnop ONot TEmpty),
    testCase "boolean not on i32" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $ Just $ Expr (EUnop ONot $ litI32 1)
            ]
        )
        (EInvalidUnop ONot TInt32),
    testCase "boolean not on f64" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TFloat64 $
                BExpr [] $ Just $ Expr (EUnop ONot $ litF64 1)
            ]
        )
        (EInvalidUnop ONot TFloat64)
  ]
