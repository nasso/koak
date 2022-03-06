module Koa.Analyser.ToMIR (program) where

import Koa.Analyser.Monad
import qualified Koa.Syntax.HIR as HIR
import qualified Koa.Syntax.MIR as MIR

program :: HIR.ProgramT -> Analyser MIR.Program
program (HIR.Program defs) = MIR.Program <$> mapM def defs

def :: HIR.DefinitionT -> Analyser MIR.Definition
def (HIR.DFn name args ty bod) =
  MIR.DFn
    <$> ident name
    <*> mapM binding args
    <*> type' ty
    <*> funBody bod

binding :: HIR.TBinding -> Analyser MIR.TBinding
binding (HIR.TBinding pat ty) = MIR.TBinding <$> pattern' pat <*> type' ty

funBody :: HIR.BlockT -> Analyser [MIR.Stmt]
funBody (HIR.BExpr stmts Nothing) = mapM stmt stmts
funBody (HIR.BExpr stmts (Just e)) =
  do
    stmts' <- mapM stmt stmts
    e' <- expr e
    pure $ stmts' ++ [MIR.SReturn e']

ident :: HIR.Ident -> Analyser MIR.Ident
ident (HIR.Ident i) = pure $ MIR.Ident i

pattern' :: HIR.Pattern -> Analyser MIR.Pattern
pattern' HIR.PWildcard = pure MIR.PWildcard
pattern' (HIR.PIdent i) = MIR.PIdent <$> ident i
pattern' (HIR.PMutIdent i) = MIR.PMutIdent <$> ident i

type' :: HIR.Type -> Analyser MIR.Type
type' HIR.TEmpty = pure MIR.TEmpty
type' HIR.TBool = pure MIR.TBool
type' HIR.TInt32 = pure MIR.TInt32
type' HIR.TFloat64 = pure MIR.TFloat64

stmt :: HIR.StmtT -> Analyser MIR.Stmt
stmt (HIR.SExpr e) = MIR.SExpr <$> expr e
stmt (HIR.SReturn e) = MIR.SReturn <$> expr e
stmt (HIR.SLet pat _ e@(HIR.ExprT (_, ty))) =
  MIR.SLet <$> pattern' pat <*> type' ty <*> expr e

expr :: HIR.ExprT -> Analyser MIR.Expr
expr (HIR.ExprT (HIR.EBlock b, _)) = MIR.EBlock <$> block b
expr (HIR.ExprT (HIR.EIdent i, ty)) = MIR.EVar <$> type' ty <*> ident i
expr (HIR.ExprT (HIR.EIf cond then' else', _)) =
  MIR.EIf <$> expr cond <*> block then' <*> block else'
expr (HIR.ExprT (HIR.EWhile cond body, _)) =
  do
    cond' <- expr cond
    body' <- block body
    pure $ MIR.ELoop $ MIR.Block [] $ MIR.EIf cond' body' breakEmpty
expr (HIR.ExprT (HIR.EFor initStmt cond update body, _)) =
  do
    initStmt' <- stmt initStmt
    cond' <- expr cond
    update' <- expr update
    body' <- block body
    pure . MIR.EBlock . MIR.Block [initStmt'] . MIR.ELoop . MIR.Block [] $
      MIR.EIf
        cond'
        (MIR.Block [MIR.SExpr $ MIR.EBlock body'] update')
        breakEmpty
expr (HIR.ExprT (HIR.ECall name args, _)) =
  MIR.ECall <$> ident name <*> mapM expr args
expr (HIR.ExprT (HIR.EAssign name e, _)) = MIR.EAssign <$> ident name <*> expr e
expr (HIR.ExprT (HIR.EBinop op lhs rhs, _)) =
  MIR.EBinop <$> typeOf lhs <*> binop op <*> expr lhs <*> expr rhs
expr (HIR.ExprT (HIR.EUnop op e, _)) = MIR.EUnop <$> unop op <*> expr e
expr (HIR.ExprT (HIR.ELit lit, ty)) = MIR.EConst <$> lit' ty lit

typeOf :: HIR.ExprT -> Analyser MIR.Type
typeOf (HIR.ExprT (_, ty)) = type' ty

lit' :: HIR.Type -> HIR.Literal -> Analyser MIR.Constant
lit' HIR.TInt32 (HIR.LInt n) = pure $ MIR.CInt32 $ fromIntegral n
lit' HIR.TFloat64 (HIR.LFloat x) = pure $ MIR.CFloat64 x
lit' HIR.TBool (HIR.LBool b) = pure $ MIR.CBool b
lit' HIR.TEmpty HIR.LEmpty = pure MIR.CEmpty
lit' t l = error $ "unsupported literal constant: " ++ show t ++ " " ++ show l

binop :: HIR.Binop -> Analyser MIR.Binop
binop HIR.OAdd = pure MIR.OAdd
binop HIR.OSub = pure MIR.OSub
binop HIR.OMul = pure MIR.OMul
binop HIR.ODiv = pure MIR.ODiv
binop HIR.OEquals = pure MIR.OEquals
binop HIR.ONotEquals = pure MIR.ONotEquals
binop HIR.OLessThan = pure MIR.OLessThan
binop HIR.OGreaterThan = pure MIR.OGreaterThan
binop HIR.OLessThanEq = pure MIR.OLessThanEq
binop HIR.OGreaterThanEq = pure MIR.OGreaterThanEq

unop :: HIR.Unop -> Analyser MIR.Unop
unop HIR.ONot = pure MIR.ONot
unop HIR.ONeg = pure MIR.ONeg

breakEmpty :: MIR.Block
breakEmpty =
  MIR.Block
    [MIR.SBreak $ MIR.EConst MIR.CEmpty]
    $ MIR.EConst MIR.CEmpty

block :: HIR.BlockT -> Analyser MIR.Block
block (HIR.BExpr stmts (Just e)) = MIR.Block <$> mapM stmt stmts <*> expr e
block (HIR.BExpr stmts Nothing) =
  do
    stmts' <- mapM stmt stmts
    pure $ MIR.Block stmts' $ MIR.EConst MIR.CEmpty
