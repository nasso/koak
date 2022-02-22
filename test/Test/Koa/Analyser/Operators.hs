module Test.Koa.Analyser.Operators (operatorsTests) where

import Koa.Analyser
import Koa.Syntax
import Test.Koa.Analyser.Util
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
                BExpr [] $
                  Expr
                    ( EBinop
                        OAdd
                        (Expr $ ELit $ LInt 1)
                        (Expr $ ELit $ LInt 2)
                    )
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $
                  ExprT
                    ( EBinop
                        OAdd
                        (ExprT (ELit $ LInt 1, TInt32))
                        (ExprT (ELit $ LInt 2, TInt32)),
                      TInt32
                    )
            ]
        ),
    testCase "i32 == i32 is a boolean" $
      assertValidProgram
        ( Program
            [ DFn (Ident "foo") [] TBool $
                BExpr [] $
                  Expr
                    ( EBinop
                        OEquals
                        (Expr $ ELit $ LInt 1)
                        (Expr $ ELit $ LInt 2)
                    )
            ]
        )
        ( Program
            [ DFn (Ident "foo") [] TBool $
                BExpr [] $
                  ExprT
                    ( EBinop
                        OEquals
                        (ExprT (ELit $ LInt 1, TInt32))
                        (ExprT (ELit $ LInt 2, TInt32)),
                      TBool
                    )
            ]
        )
  ]

invalidPrograms :: [TestTree]
invalidPrograms =
  [ testCase "can't add together i32 and f64" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TInt32 $
                BExpr [] $
                  Expr
                    ( EBinop
                        OAdd
                        (Expr $ ELit $ LInt 1)
                        (Expr $ ELit $ LFloat 2.0)
                    )
            ]
        )
        (EInvalidBinop OAdd TInt32 TFloat64),
    testCase "can't add empties together" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TEmpty $
                BExpr [] $
                  Expr
                    ( EBinop
                        OAdd
                        (Expr $ ELit LEmpty)
                        (Expr $ ELit LEmpty)
                    )
            ]
        )
        (EInvalidBinop OAdd TEmpty TEmpty),
    testCase "can't use < on empties" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TBool $
                BExpr [] $
                  Expr
                    ( EBinop
                        OLessThan
                        (Expr $ ELit LEmpty)
                        (Expr $ ELit LEmpty)
                    )
            ]
        )
        (EInvalidBinop OLessThan TEmpty TEmpty),
    testCase "can't use < on booleans" $
      assertInvalidProgram
        ( Program
            [ DFn (Ident "foo") [] TBool $
                BExpr [] $
                  Expr
                    ( EBinop
                        OLessThan
                        (Expr $ ELit $ LBool True)
                        (Expr $ ELit $ LBool False)
                    )
            ]
        )
        (EInvalidBinop OLessThan TBool TBool)
  ]
