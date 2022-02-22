module Test.Koa.Analyser.Util where

import Koa.Analyser
import Koa.Syntax
import Test.Tasty.HUnit

defaultConfig :: AnalyserConfig
defaultConfig = AnalyserConfig {cfgTreatWarningsAsErrors = False}

assertValidProgram :: Program -> ProgramT -> Assertion
assertValidProgram ast checked =
  case analyseProgram defaultConfig ast of
    Right (c, _) -> c @?= checked
    Left e -> assertFailure $ "Analyser failed: " ++ show e

assertInvalidProgram :: Program -> AnalyserErrorType -> Assertion
assertInvalidProgram ast expectedErr =
  case analyseProgram defaultConfig ast of
    Left (e, _) -> e @?= expectedErr
    Right _ -> assertFailure "Analyser succeeded"

class FromLit a where
  litEmpty :: a
  litBool :: Bool -> a
  litI32 :: Integer -> a
  litF64 :: Double -> a

instance FromLit Expr where
  litEmpty = Expr $ ELit LEmpty
  litBool = Expr . ELit . LBool
  litI32 = Expr . ELit . LInt
  litF64 = Expr . ELit . LFloat

instance FromLit ExprT where
  litEmpty = ExprT (ELit LEmpty, TEmpty)
  litBool b = ExprT (ELit $ LBool b, TBool)
  litI32 n = ExprT (ELit $ LInt n, TInt32)
  litF64 r = ExprT (ELit $ LFloat r, TFloat64)
