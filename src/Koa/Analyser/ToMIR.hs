module Koa.Analyser.ToMIR (program) where

import Koa.Analyser.Monad
import qualified Koa.Syntax.HIR as HIR
import qualified Koa.Syntax.MIR as MIR

program :: HIR.ProgramT -> Analyser MIR.Program
program (HIR.Program defs) = MIR.Program <$> mapM def defs

def :: HIR.DefinitionT -> Analyser MIR.Definition
def (HIR.DFn name bindings ty bod) =
  do
    name' <- ident name
    (argNames, argTys, argInits) <- unzip3 <$> allBindings bindings
    ty' <- type' ty
    bod' <- body bod
    pure $ MIR.DFn name' (zip argNames argTys) ty' $ argInits ++ bod'

ident :: HIR.Ident -> Analyser MIR.Ident
ident (HIR.Ident i) = pure $ MIR.Ident i

allBindings :: [HIR.TBinding] -> Analyser [(MIR.Ident, MIR.Type, MIR.Stmt)]
allBindings bindings =
  mapM namedBinding namedBindings
  where
    names = MIR.Ident . ("arg" ++) . show <$> [0 :: Word ..]
    namedBindings = zip names bindings
    namedBinding (name, b) = uncurry ((,,) name) <$> binding name b

binding :: MIR.Ident -> HIR.TBinding -> Analyser (MIR.Type, MIR.Stmt)
binding name (HIR.TBinding pat ty) =
  do
    ty' <- type' ty
    pat' <- pattern' pat
    pure (ty', MIR.SLet pat' $ MIR.EVar ty' name)

pattern' :: HIR.Pattern -> Analyser MIR.Pattern
pattern' HIR.PWildcard = pure MIR.PWildcard
pattern' (HIR.PIdent i) = MIR.PIdent <$> ident i
pattern' (HIR.PMutIdent i) = MIR.PMutIdent <$> ident i

type' :: HIR.Type -> Analyser MIR.Type
type' HIR.TEmpty = pure MIR.TEmpty
type' HIR.TBool = pure MIR.TBool
type' HIR.TInt32 = pure MIR.TInt32
type' HIR.TFloat64 = pure MIR.TFloat64

body :: HIR.BlockT -> Analyser [MIR.Stmt]
body (HIR.BExpr stmts Nothing) = mapM stmt stmts
body (HIR.BExpr stmts (Just e)) =
  do
    stmts' <- mapM stmt stmts
    e' <- expr e
    pure $ stmts' ++ [MIR.SReturn e']

stmt :: HIR.StmtT -> Analyser MIR.Stmt
stmt = error "not implemented"

expr :: HIR.ExprT -> Analyser MIR.Expr
expr (HIR.ExprT (HIR.EBlock b, _)) = MIR.EBlock <$> block b
expr (HIR.ExprT (HIR.EIdent {}, _)) = error "not implemented: ToMIR.EIdent"
expr (HIR.ExprT (HIR.EIf {}, _)) = error "not implemented: ToMIR.EIf"
expr (HIR.ExprT (HIR.EWhile {}, _)) = error "not implemented: ToMIR.EWhile"
expr (HIR.ExprT (HIR.EFor {}, _)) = error "not implemented: ToMIR.EFor"
expr (HIR.ExprT (HIR.ECall {}, _)) = error "not implemented: ToMIR.ECall"
expr (HIR.ExprT (HIR.EAssign {}, _)) = error "not implemented: ToMIR.EAssign"
expr (HIR.ExprT (HIR.EBinop {}, _)) = error "not implemented: ToMIR.EBinop"
expr (HIR.ExprT (HIR.EUnop {}, _)) = error "not implemented: ToMIR.EUnop"
expr (HIR.ExprT (HIR.ELit {}, _)) = error "not implemented: ToMIR.ELit"

block :: HIR.BlockT -> Analyser MIR.Block
block (HIR.BExpr stmts (Just e)) = MIR.Block <$> mapM stmt stmts <*> expr e
block (HIR.BExpr stmts Nothing) =
  do
    stmts' <- mapM stmt stmts
    pure $ MIR.Block stmts' $ MIR.EConst MIR.CEmpty
