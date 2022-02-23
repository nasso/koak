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
import Control.Monad.Reader
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Writer.Strict
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Koa.Syntax

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
type AnalyserResult =
  Either AnalyserError (ProgramT, [AnalyserWarning])

data Mutability = Mutable | Immutable deriving (Show, Eq)

data SymbolBinding
  = STFun [Type] Type
  | STVar Mutability Type
  deriving (Show, Eq)

-- | The read-only environment in which the analyser runs.
data AnalyserEnv = AnalyserEnv
  { envCfg :: AnalyserConfig,
    envCtx :: HashMap String SymbolBinding
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

-- | Runs the static analyser on the given program AST.
analyseProgram :: AnalyserConfig -> Program -> AnalyserResult
analyseProgram cfg ast =
  runExcept $
    runWriterT $
      runReaderT (runAnalyser $ program ast) $
        AnalyserEnv {envCfg = cfg, envCtx = HM.empty}

-- | Analyse a program.
program :: Program -> Analyser ProgramT
program (Program defs) =
  withAllDefs defs (Program <$> traverse definition defs)

withAllDefs :: [Definition] -> Analyser a -> Analyser a
withAllDefs ds a = foldr withDef a ds

withDef :: Definition -> Analyser a -> Analyser a
withDef (DFn (Ident name) args rety _) = local insertDef
  where
    argtys = (\(TBinding _ ty) -> ty) <$> args
    insertDef e = e {envCtx = HM.insert name (STFun argtys rety) $ envCtx e}

-- | Analyse a definition.
definition :: Definition -> Analyser DefinitionT
definition (DFn name args rety body) =
  DFn name args rety <$> withBindings args (checkBlock rety body)

-- | Run a computation with a set of bindings.
withBindings :: [TBinding] -> Analyser a -> Analyser a
withBindings bindings =
  local insertBindings
  where
    insertBindings e = e {envCtx = HM.union newBindings $ envCtx e}
    newBindings = HM.fromList . catMaybes $ binding <$> bindings
    binding (TBinding (PIdent (Ident n)) ty) = Just (n, STVar Immutable ty)
    binding (TBinding (PMutIdent (Ident n)) ty) = Just (n, STVar Mutable ty)
    binding (TBinding PWildcard _) = Nothing

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
block (BExpr stmts e) =
  withStatements stmts $ \stmts' -> BExpr stmts' <$> expression e

-- | Analyse a series of statements and run a computation in the modified scope.
withStatements :: [Stmt] -> ([StmtT] -> Analyser a) -> Analyser a
withStatements [] k = k []
withStatements (s : ss) k =
  withStatement s $
    \s' -> withStatements ss $
      \ss' -> k (s' : ss')

-- | Analyse a statement and run a computation in the modified scope.
withStatement :: Stmt -> (StmtT -> Analyser a) -> Analyser a
withStatement (SExpr expr) k = expression expr >>= k . SExpr
withStatement s@(SLet pat explicitTy e) k =
  do
    e'@(ExprT (_, exprTy)) <- expression e
    case explicitTy of
      Just ty | ty /= exprTy -> throwError (ETypeMismatch ty exprTy, LStmt s)
      _ -> pure ()
    local (bind pat e') $ k $ SLet pat (Just exprTy) e'
withStatement _ _ = error "not implemented: withStatement"

bind :: Pattern -> ExprT -> AnalyserEnv -> AnalyserEnv
bind (PIdent (Ident varname)) (ExprT (_, ty)) env =
  env {envCtx = HM.insert varname (STVar Immutable ty) $ envCtx env}
bind (PMutIdent (Ident varname)) (ExprT (_, ty)) env =
  env {envCtx = HM.insert varname (STVar Mutable ty) $ envCtx env}
bind PWildcard _ env = env

-- | Analyse an expression.
expression :: Expr -> Analyser ExprT
expression (Expr (ELit lit)) = pure $ ExprT (ELit lit, litType lit)
expression e@(Expr (ECall fn args)) =
  do
    args' <- traverse expression args
    let argsTy = (\(ExprT (_, ty)) -> ty) <$> args'
    (paramTys, rety) <- lookupFunction fn
    when (paramTys /= argsTy) $ throwError (EInvalidArguments, LExpr e)
    pure $ ExprT (ECall fn args', rety)
expression e@(Expr (EBinop op lhs rhs)) =
  do
    lhs'@(ExprT (_, lhsTy)) <- expression lhs
    rhs'@(ExprT (_, rhsTy)) <- expression rhs
    case binopOutput op lhsTy rhsTy of
      Nothing -> throwError (EInvalidBinop op lhsTy rhsTy, LExpr e)
      Just outputTy -> pure $ ExprT (EBinop op lhs' rhs', outputTy)
expression (Expr (EUnop _ _)) = error "not implemented: expression EUnop"
expression e@(Expr (EIf cond then' else')) =
  do
    cond'@(ExprT (_, condTy)) <- expression cond
    when (condTy /= TBool) $ throwError (ETypeMismatch TBool condTy, LExpr cond)
    then''@(ExprT (_, thenTy)) <- expression then'
    else''@(ExprT (_, elseTy)) <- expression else'
    when (thenTy /= elseTy) $ throwError (ETypeMismatch thenTy elseTy, LExpr e)
    pure $ ExprT (EIf cond' then'' else'', thenTy)
expression (Expr (EWhile cond body)) =
  do
    cond'@(ExprT (_, condTy)) <- expression cond
    when (condTy /= TBool) $ throwError (ETypeMismatch TBool condTy, LExpr cond)
    body' <- block body
    pure $ ExprT (EWhile cond' body', blockType body')
expression (Expr (EIdent varname)) =
  do
    (_, ty) <- lookupVar varname
    pure $ ExprT (EIdent varname, ty)
expression e@(Expr (EAssign varname val)) =
  do
    (mut, ty) <- lookupVar varname
    when (mut == Immutable) $
      throwError (EMutationOfImmutable varname, LExpr e)
    val'@(ExprT (_, valTy)) <- expression val
    when (ty /= valTy) $
      throwError (ETypeMismatch ty valTy, LExpr e)
    pure $ ExprT (EAssign varname val', ty)
expression (Expr (EBlock b)) =
  do
    b' <- block b
    pure $ ExprT (EBlock b', blockType b')
expression (Expr EFor {}) = error "not implemented: expression EFor"

-- | Look up a function in the analyser context.
lookupFunction :: Ident -> Analyser ([Type], Type)
lookupFunction ident@(Ident name) =
  do
    ctx <- asks envCtx
    case HM.lookup name ctx of
      Just (STFun args rety) -> pure (args, rety)
      Just _ -> throwError (ENotAFunction ident, LIdent ident)
      Nothing -> throwError (EUndefinedSymbol ident, LIdent ident)

-- | Look up a variable in the analyser context.
lookupVar :: Ident -> Analyser (Mutability, Type)
lookupVar ident@(Ident name) =
  do
    ctx <- asks envCtx
    case HM.lookup name ctx of
      Just (STVar mut ty) -> pure (mut, ty)
      Just _ -> throwError (ENotAVariable ident, LIdent ident)
      Nothing -> throwError (EUndefinedSymbol ident, LIdent ident)

-- | Get the type of a block.
blockType :: BlockT -> Type
blockType (BExpr _ (ExprT (_, ty))) = ty

-- | Get the type of a literal.
--
-- TODO: the type of a literal can be ambiguous, e.g. @1@ can be an integer of
-- any size. We could infer the type from the context in which it is used, or
-- return a list of the possible types.
litType :: Literal -> Type
litType LEmpty = TEmpty
litType (LInt _) = TInt32
litType (LFloat _) = TFloat64
litType (LBool _) = TBool

-- | The output type of a binary operator, given the types of its operands.
binopOutput :: Binop -> Type -> Type -> Maybe Type
binopOutput OEquals lhs rhs | lhs == rhs = Just TBool
binopOutput ONotEquals lhs rhs | lhs == rhs = Just TBool
binopOutput OAdd lhs rhs | isNum lhs && lhs == rhs = Just lhs
binopOutput OSub lhs rhs | isNum lhs && lhs == rhs = Just lhs
binopOutput OMul lhs rhs | isNum lhs && lhs == rhs = Just lhs
binopOutput ODiv lhs rhs | isNum lhs && lhs == rhs = Just lhs
binopOutput OGreaterThan lhs rhs | isOrd lhs && lhs == rhs = Just TBool
binopOutput OGreaterThanEq lhs rhs | isOrd lhs && lhs == rhs = Just TBool
binopOutput OLessThan lhs rhs | isOrd lhs && lhs == rhs = Just TBool
binopOutput OLessThanEq lhs rhs | isOrd lhs && lhs == rhs = Just TBool
binopOutput _ _ _ = Nothing

-- | Determine if a type is a numeric type.
isNum :: Type -> Bool
isNum TEmpty = False
isNum TBool = False
isNum TInt32 = True
isNum TFloat64 = True

-- | Determine if a type is an ordinal type.
isOrd :: Type -> Bool
isOrd TEmpty = False
isOrd TBool = False
isOrd TInt32 = True
isOrd TFloat64 = True
