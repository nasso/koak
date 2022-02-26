module Koa.Analyser.TypeCheck
  ( program,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Koa.Analyser.Monad
import Koa.Syntax.HIR

-- | Analyse a program.
program :: Program -> Analyser ProgramT
program (Program defs) =
  withAllDefs defs (Program <$> traverse definition defs)

withAllDefs :: [Definition] -> Analyser a -> Analyser a
withAllDefs ds a = foldr withDef a ds

withDef :: Definition -> Analyser a -> Analyser a
withDef (DFn (Ident name) args rety _) =
  local insertDef
  where
    argtys = (\(TBinding _ ty) -> ty) <$> args
    insertDef e = e {envCtx = HM.insert name (STFun argtys rety) $ envCtx e}

-- | Analyse a definition.
definition :: Definition -> Analyser DefinitionT
definition (DFn name args rety body) =
  DFn name args rety <$> withBindings args (funBody body rety)

-- | Analyse a block, verifying that it evaluates to the expected type.
funBody :: Block -> Type -> Analyser BlockT
funBody b@(BExpr _ le) rety =
  local (\e -> e {envFunType = Just rety}) $
    case le of
      Nothing ->
        unless
          (rety == TEmpty || blockUnconditionnallyReturns b)
          (throwError (ETypeMismatch rety TEmpty, LBlock b))
          >> block b
      _ -> blockWithType b rety

-- | Figure out if a block unconditionally returns.
blockUnconditionnallyReturns :: Block -> Bool
blockUnconditionnallyReturns (BExpr stmts (Just e)) =
  any stmtUnconditionnallyReturns stmts || exprUnconditionnallyReturns e
blockUnconditionnallyReturns (BExpr stmts Nothing) =
  any stmtUnconditionnallyReturns stmts

-- | Figure out if a statement unconditionally returns.
stmtUnconditionnallyReturns :: Stmt -> Bool
stmtUnconditionnallyReturns (SReturn _) = True
stmtUnconditionnallyReturns (SLet _ _ e) = exprUnconditionnallyReturns e
stmtUnconditionnallyReturns (SExpr e) = exprUnconditionnallyReturns e

-- | Figure out if an expression unconditionally returns.
exprUnconditionnallyReturns :: Expr -> Bool
exprUnconditionnallyReturns (Expr (EBlock b)) = blockUnconditionnallyReturns b
exprUnconditionnallyReturns (Expr (EIdent _)) = False
exprUnconditionnallyReturns (Expr (EIf _ then' else')) =
  blockUnconditionnallyReturns then' && blockUnconditionnallyReturns else'
exprUnconditionnallyReturns (Expr (EWhile e _)) = exprUnconditionnallyReturns e
exprUnconditionnallyReturns (Expr (EFor i c _ _)) =
  stmtUnconditionnallyReturns i || exprUnconditionnallyReturns c
exprUnconditionnallyReturns (Expr (ECall _ args)) =
  any exprUnconditionnallyReturns args
exprUnconditionnallyReturns (Expr (EAssign _ e)) = exprUnconditionnallyReturns e
exprUnconditionnallyReturns (Expr (EBinop _ lhs rhs)) =
  exprUnconditionnallyReturns lhs || exprUnconditionnallyReturns rhs
exprUnconditionnallyReturns (Expr (EUnop _ e)) = exprUnconditionnallyReturns e
exprUnconditionnallyReturns (Expr (ELit _)) = False

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

-- | Analyse a block.
block :: Block -> Analyser BlockT
block (BExpr stmts (Just e)) =
  withStatements stmts $ \stmts' -> BExpr stmts' . Just <$> expression e
block (BExpr stmts Nothing) =
  withStatements stmts $ \stmts' -> pure $ BExpr stmts' Nothing

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
      _ -> local (bind pat e') $ k $ SLet pat (Just exprTy) e'
withStatement s@(SReturn e) k =
  do
    e'@(ExprT (_, ty)) <- expression e
    retTy <- asks envFunType
    case retTy of
      Just ty' | ty /= ty' -> throwError (ETypeMismatch ty' ty, LStmt s)
      _ -> k $ SReturn e'

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
expression e@(Expr (EUnop op val)) =
  do
    val'@(ExprT (_, valTy)) <- expression val
    case unopOutput op valTy of
      Nothing -> throwError (EInvalidUnop op valTy, LExpr e)
      Just outputTy -> pure $ ExprT (EUnop op val', outputTy)
expression (Expr (EIf cond then' else')) =
  do
    cond'@(ExprT (_, condTy)) <- expression cond
    when (condTy /= TBool) $ throwError (ETypeMismatch TBool condTy, LExpr cond)
    then'' <- block then'
    let branchTy = blockType then''
    else'' <- blockWithType else' branchTy
    pure $ ExprT (EIf cond' then'' else'', branchTy)
expression (Expr (EWhile cond body)) =
  do
    cond'@(ExprT (_, condTy)) <- expression cond
    when (condTy /= TBool) $ throwError (ETypeMismatch TBool condTy, LExpr cond)
    body' <- blockWithType body TEmpty
    pure $ ExprT (EWhile cond' body', TEmpty)
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
expression (Expr (EFor initStmt condExpr updateExpr body)) =
  withStatement initStmt $ \initStmt' ->
    do
      condExpr'@(ExprT (_, condTy)) <- expression condExpr
      updateExpr' <- expression updateExpr
      when (condTy /= TBool) $
        throwError (ETypeMismatch TBool condTy, LExpr condExpr)
      body' <- blockWithType body TEmpty
      pure $ ExprT (EFor initStmt' condExpr' updateExpr' body', TEmpty)

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

-- | Analyse a block, verifying that it evaluates to the expected type.
blockWithType :: Block -> Type -> Analyser BlockT
blockWithType b expectedTy =
  do
    b' <- block b
    let actualTy = blockType b'
    when (actualTy /= expectedTy) $
      throwError (ETypeMismatch expectedTy actualTy, LBlock b)
    pure b'

-- | Get the type of a block.
blockType :: BlockT -> Type
blockType (BExpr _ (Just (ExprT (_, ty)))) = ty
blockType (BExpr _ Nothing) = TEmpty

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

-- | The output type of a unary operator, given the type of its operand.
unopOutput :: Unop -> Type -> Maybe Type
unopOutput ONot TBool = Just TBool
unopOutput ONeg ty | isNum ty = Just ty
unopOutput _ _ = Nothing

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
