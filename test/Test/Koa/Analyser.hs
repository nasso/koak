module Test.Koa.Analyser (analyserTests) where

import Koa.Analyser
import Koa.Syntax
import Test.Tasty
import Test.Tasty.HUnit

analyserTests :: [TestTree]
analyserTests =
  [ testGroup "Valid" validPrograms,
    testGroup "Invalid" invalidPrograms
  ]

validPrograms :: [TestTree]
validPrograms =
  [ testCase "empty" $ assertValidProgram (Program []) (Program []),
    testCase "main returning empty" $
      assertValidProgram
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
      assertValidProgram
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

invalidPrograms :: [TestTree]
invalidPrograms =
  [ testCase "return type mismatch" $
      assertInvalidProgram
        ( Program
            [ DFn
                (Ident "main")
                []
                TEmpty
                (BExpr [] $ Expr $ ELit $ LInt 0)
            ]
        )
        (ETypeMismatch TEmpty TInt32)
  ]

defaultConfig :: AnalyserConfig
defaultConfig = AnalyserConfig {treatWarningsAsErrors = False}

assertValidProgram :: Program -> ProgramT -> Assertion
assertValidProgram ast checked =
  case analyseProgram defaultConfig ast of
    Right (c, _) -> c @?= checked
    _ -> assertFailure "Analyser failed"

assertInvalidProgram :: Program -> AnalyserErrorType -> Assertion
assertInvalidProgram ast expectedErr =
  case analyseProgram defaultConfig ast of
    Left (e, _) -> e @?= expectedErr
    Right _ -> assertFailure "Analyser succeeded"
