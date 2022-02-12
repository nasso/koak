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

import Data.List.NonEmpty (NonEmpty)
import Koa.Syntax

-- | Configuration for the analyser.
newtype AnalyserConfig = AnalyserConfig
  { treatWarningsAsErrors :: Bool
  }
  deriving (Show, Eq)

data AnalyserWarningType
  = WDeadCode
  deriving (Show, Eq)

data AnalyserErrorType
  = EUndefinedSymbol Ident
  | ETypeMismatch {eExpected :: Type, eActual :: Type}
  | EWarn AnalyserWarningType
  deriving (Show, Eq)

data AnalyserLocation
  = LDef Definition
  | LExpr Expr
  | LStmt Stmt
  | LIdent Ident
  deriving (Show, Eq)

type AnalyserWarning = (AnalyserWarningType, AnalyserLocation)

type AnalyserError = (AnalyserErrorType, AnalyserLocation)

type AnalyserResult =
  Either (NonEmpty AnalyserError) (ProgramT, [AnalyserWarning])

analyseProgram :: AnalyserConfig -> Program -> AnalyserResult
analyseProgram _ (Program []) = Right (Program [], [])
analyseProgram _ _ = error "Not implemented"
