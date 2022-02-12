module Test.Koa.Analyser (analyserTests) where

import Koa.Analyser
import Koa.Syntax
import Test.Tasty
import Test.Tasty.HUnit

analyserTests :: [TestTree]
analyserTests =
  [ testCase "empty" $ assertProgram (Program []) (Program []),
    testCase "main returning empty" $
      assertProgram
        ( Program
            [ DFn
                (Ident "main")
                []
                TEmpty
                (BExpr [] $ Expr $ ELit LEmpty)
            ]
        )
        ( Program
            [ DFn
                (Ident "main")
                []
                TEmpty
                (BExpr [] $ ExprT (ELit LEmpty, TEmpty))
            ]
        ),
    testCase "main returning zero" $
      assertProgram
        ( Program
            [ DFn
                (Ident "main")
                []
                TInt32
                (BExpr [] $ Expr $ ELit $ LInt 0)
            ]
        )
        ( Program
            [ DFn
                (Ident "main")
                []
                TInt32
                (BExpr [] $ ExprT (ELit $ LInt 0, TInt32))
            ]
        )
  ]

assertProgram :: Program -> ProgramT -> Assertion
assertProgram ast checked = analyseProgram AnalyserConfig ast @?= Right checked
