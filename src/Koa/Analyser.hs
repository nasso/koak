{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Writer.Strict
import Koa.Syntax

-- | Configuration for the analyser.
newtype AnalyserConfig = AnalyserConfig
  { treatWarningsAsErrors :: Bool
  }
  deriving (Show, Eq)

-- | A warning that can be emitted by the analyser.
data AnalyserWarningType
  = WDeadCode
  deriving (Show, Eq)

-- | An error indicating that the program is invalid.
data AnalyserErrorType
  = EUndefinedSymbol Ident
  | ETypeMismatch {eExpected :: Type, eActual :: Type}
  | EWarn AnalyserWarningType
  | EUnimplemented
  deriving (Show, Eq)

-- | A location in the source code.
data AnalyserLocation
  = LDef Definition
  | LExpr Expr
  | LStmt Stmt
  | LIdent Ident
  deriving (Show, Eq)

-- | A warning at a specific location.
type AnalyserWarning = (AnalyserWarningType, AnalyserLocation)

-- | An error at a specific location.
type AnalyserError = (AnalyserErrorType, AnalyserLocation)

-- | The result of the analyser.
type AnalyserResult =
  Either AnalyserError (ProgramT, [AnalyserWarning])

-- | A monad representing the analysis of an AST.
newtype Analyser a = Analyser
  { runAnalyser ::
      ReaderT
        AnalyserConfig -- Reads configuration
        ( WriterT
            [AnalyserWarning] -- Emits warnings
            (Except AnalyserError) -- Fails on error
        )
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader AnalyserConfig,
      MonadWriter [AnalyserWarning],
      MonadError AnalyserError
    )

analyseProgram :: AnalyserConfig -> Program -> AnalyserResult
analyseProgram cfg ast =
  runExcept $ runWriterT $ runReaderT (runAnalyser $ program ast) cfg

program :: Program -> Analyser ProgramT
program (Program []) = pure (Program [])
program (Program (d : _)) = throwError (EUnimplemented, LDef d)
