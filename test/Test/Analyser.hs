module Test.Analyser (analyserTests) where

import Analyser
import Syntax
import Test.Tasty
import Test.Tasty.HUnit

analyserTests :: [TestTree]
analyserTests =
  [ testCase "empty" $ assertProgram (Program []) (Program []),
    testCase "minimal" $
      assertProgram
        ( Program
            [ DFn
                (Ident "main")
                []
                TInt32
                (BExpr [] $ Just $ Expr $ ELit $ LInt 0)
            ]
        )
        ( Program
            [ DFn
                (Ident "main")
                []
                TInt32
                (BExpr [] $ Just $ ExprT (ELit $ LInt 0, TInt32))
            ]
        )
  ]

assertProgram :: Program -> ProgramT -> Assertion
assertProgram ast checked = analyseProgram AnalyserConfig ast @?= Right checked
