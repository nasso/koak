{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Koa.Analyser.Monad where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.HashMap.Strict (HashMap)
import Koa.Syntax.HIR

-- | Configuration for the analyser.
newtype AnalyserConfig = AnalyserConfig
  { cfgTreatWarningsAsErrors :: Bool
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
  | EInvalidBinop Binop Type Type
  | EInvalidUnop Unop Type
  | EMutationOfImmutable Ident
  | EWarn AnalyserWarningType
  | ENotAFunction Ident
  | ENotAVariable Ident
  | EInvalidArguments
  | EUnimplemented
  deriving (Show, Eq)

-- | A location in the source code.
data AnalyserLocation
  = LDef Definition
  | LExpr Expr
  | LStmt Stmt
  | LIdent Ident
  | LBlock Block
  deriving (Show, Eq)

-- | A warning at a specific location.
type AnalyserWarning = (AnalyserWarningType, AnalyserLocation)

-- | An error at a specific location.
type AnalyserError = (AnalyserErrorType, AnalyserLocation)

-- | The result of the analyser.
type AnalyserResult a = Either AnalyserError (a, [AnalyserWarning])

data Mutability = Mutable | Immutable deriving (Show, Eq)

data SymbolBinding
  = STFun [Type] Type
  | STVar Mutability Type
  deriving (Show, Eq)

-- | The read-only environment in which the analyser runs.
data AnalyserEnv = AnalyserEnv
  { envCfg :: AnalyserConfig,
    envCtx :: HashMap String SymbolBinding,
    envFunType :: Maybe Type
  }
  deriving (Show, Eq)

-- | A monad representing the analysis of an AST.
newtype Analyser a = Analyser
  { runAnalyser ::
      ReaderT
        AnalyserEnv
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
      MonadReader AnalyserEnv,
      MonadWriter [AnalyserWarning],
      MonadError AnalyserError
    )
