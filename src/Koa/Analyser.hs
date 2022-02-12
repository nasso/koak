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
  | LBlock Block
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

-- | Runs the static analyser on the given program AST.
analyseProgram :: AnalyserConfig -> Program -> AnalyserResult
analyseProgram cfg ast =
  runExcept $ runWriterT $ runReaderT (runAnalyser $ program ast) cfg

-- | Analyse a program.
program :: Program -> Analyser ProgramT
program (Program defs) = Program <$> traverse definition defs

-- | Analyse a definition.
definition :: Definition -> Analyser DefinitionT
definition (DFn name args rety body) =
  DFn name args rety <$> checkBlock rety body

-- | Analyse a block, verifying that it evaluates to the expected type.
checkBlock :: Type -> Block -> Analyser BlockT
checkBlock expectedTy b =
  do
    block' <- block b
    let actualTy = blockType block'
    when (actualTy /= expectedTy) $
      throwError
        ( ETypeMismatch {eExpected = expectedTy, eActual = actualTy},
          LBlock b
        )
    pure block'

-- | Analyse a block.
block :: Block -> Analyser BlockT
block (BExpr stmts mExpr) =
  do
    stmts' <- traverse statement stmts
    expr' <- traverse expression mExpr
    pure $ BExpr stmts' expr'

-- | Analyse a statement.
statement :: Stmt -> Analyser StmtT
statement = error "not implemented"

-- | Analyse an expression.
expression :: Expr -> Analyser ExprT
expression (Expr (ELit lit)) = pure $ ExprT (ELit lit, litType lit)
expression _ = error "not implemented"

-- | Get the type of a block.
blockType :: BlockT -> Type
blockType (BExpr _ Nothing) = TEmpty
blockType (BExpr _ (Just e)) = exprType e

-- | Get the type of an expression.
exprType :: ExprT -> Type
exprType (ExprT (_, t)) = t

-- | Get the type of a literal.
--
-- TODO: the type of a literal can be ambiguous, e.g. @1@ can be an integer of
-- any size. We could infer the type from the context in which it is used, or
-- return a list of the possible types.
litType :: Literal -> Type
litType LEmpty = TEmpty
litType (LInt _) = TInt32
litType (LFloat _) = TFloat64
