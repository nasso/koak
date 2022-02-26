module Test.Koa.Parser.Unop (unopTests) where

import Koa.Syntax.HIR
import Test.Koa.Parser.Utils
import Test.Tasty
import Test.Tasty.HUnit

validSimple :: [TestTree]
validSimple =
  [ testCase "pos" $
      assertExpr "+ 5" $ Expr $ ELit (LInt 5),
    testCase "neg" $
      assertExpr "- 5" $
        Expr $
          EUnop
            ONeg
            $ Expr (ELit (LInt 5)),
    testCase "not" $
      assertExpr "! 5" $
        Expr $
          EUnop
            ONot
            $ Expr (ELit (LInt 5))
  ]

validComplex :: [TestTree]
validComplex =
  [ testCase "multi pos" $
      assertExpr "++5" $ Expr $ ELit (LInt 5),
    testCase "multi neg" $
      assertExpr "--5" $
        Expr $
          EUnop
            ONeg
            $ Expr $
              EUnop
                ONeg
                $ Expr (ELit (LInt 5)),
    testCase "multi not" $
      assertExpr "!!5" $
        Expr $
          EUnop
            ONot
            $ Expr $
              EUnop
                ONot
                $ Expr (ELit (LInt 5)),
    testCase "multi all" $
      assertExpr "-+-+5" $
        Expr $
          EUnop
            ONeg
            $ Expr $
              EUnop
                ONeg
                $ Expr (ELit (LInt 5)),
    testCase "in binop 1" $
      assertExpr "+4 + +5" $
        Expr $
          EBinop
            OAdd
            (Expr (ELit (LInt 4)))
            (Expr (ELit (LInt 5))),
    testCase "in binop 2" $
      assertExpr "-4 + -5" $
        Expr $
          EBinop
            OAdd
            ( Expr $
                EUnop
                  ONeg
                  $ Expr (ELit (LInt 4))
            )
            ( Expr $
                EUnop
                  ONeg
                  $ Expr (ELit (LInt 5))
            )
  ]

unopTests :: [TestTree]
unopTests =
  [ testGroup "Simple" validSimple,
    testGroup "Complex" validComplex
  ]
