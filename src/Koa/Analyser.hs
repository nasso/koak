module Koa.Analyser
  ( AnalyserConfig (..),
    AnalyserWarningType (..),
    AnalyserErrorType (..),
    AnalyserLocation (..),
    AnalyserWarning,
    AnalyserError,
    AnalyserResult,
    analyseProgram,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.HashMap.Strict as HM
import Koa.Analyser.Monad
import qualified Koa.Analyser.ToMIR as ToMIR
import qualified Koa.Analyser.TypeCheck as TC
import qualified Koa.Syntax.HIR as HIR
import qualified Koa.Syntax.MIR as MIR

-- | Runs the static analyser on the given program AST.
analyseProgram :: AnalyserConfig -> HIR.Program -> AnalyserResult MIR.Program
analyseProgram cfg ast =
  runExcept $
    runWriterT $
      runReaderT (runAnalyser $ program ast) $
        AnalyserEnv {envCfg = cfg, envCtx = HM.empty, envFunType = Nothing}

program :: HIR.Program -> Analyser MIR.Program
program hir = TC.program hir >>= ToMIR.program
