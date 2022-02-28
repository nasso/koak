module Koa.Analyser
  ( AnalyserConfig (..),
    AnalyserWarningType (..),
    AnalyserErrorType (..),
    AnalyserLocation (..),
    AnalyserWarning,
    AnalyserError,
    AnalyserResult,
    typecheckProgram,
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
analyseProgram =
  analyseWith checkAndTranslate
  where
    checkAndTranslate hir = TC.program hir >>= ToMIR.program

-- | Runs the type checker on the given program AST.
typecheckProgram :: AnalyserConfig -> HIR.Program -> AnalyserResult HIR.ProgramT
typecheckProgram = analyseWith TC.program

analyseWith ::
  (HIR.Program -> Analyser a) ->
  AnalyserConfig ->
  HIR.Program ->
  AnalyserResult a
analyseWith p cfg ast =
  runExcept $
    runWriterT $
      runReaderT (runAnalyser $ p ast) $
        AnalyserEnv {envCfg = cfg, envCtx = HM.empty, envFunType = Nothing}
