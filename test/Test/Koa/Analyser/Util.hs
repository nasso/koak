module Test.Koa.Analyser.Util where

import Koa.Analyser
import qualified Koa.Syntax.HIR as HIR
import qualified Koa.Syntax.MIR as MIR
import Test.Tasty.HUnit

defaultConfig :: AnalyserConfig
defaultConfig = AnalyserConfig {cfgTreatWarningsAsErrors = False}

assertValidProgram :: HIR.Program -> MIR.Program -> Assertion
assertValidProgram ast checked =
  case analyseProgram defaultConfig ast of
    Right (c, _) -> c @?= checked
    Left e -> assertFailure $ "Analyser failed: " ++ show e

assertInvalidProgram :: HIR.Program -> AnalyserErrorType -> Assertion
assertInvalidProgram ast expectedErr =
  case analyseProgram defaultConfig ast of
    Left (e, _) -> e @?= expectedErr
    Right _ -> assertFailure "Analyser succeeded"
