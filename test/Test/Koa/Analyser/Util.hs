module Test.Koa.Analyser.Util where

import Koa.Analyser
import Koa.Syntax.HIR
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
