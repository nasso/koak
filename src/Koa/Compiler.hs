{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Koa.Compiler
  ( CompilerConfig (..),
    OutputFormat (..),
    compileProgramToFile,
  )
where

import Control.Monad.State
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.String (IsString (fromString))
import Koa.Syntax.MIR
import qualified LLVM.AST as AST
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.Type as LLVMType
import LLVM.Context
import LLVM.IRBuilder
import LLVM.Module
import LLVM.Target
import System.Directory

-- | Configuration for the compiler.
newtype CompilerConfig = CompilerConfig
  { cfgFormat :: OutputFormat
  }
  deriving (Show, Eq)

-- | Output format for the compiled program.
data OutputFormat = Assembly | NativeObject deriving (Show, Eq)

newtype Vars = Vars
  { operands :: HashMap String AST.Operand
  }
  deriving (Eq, Show)

newVars :: Vars
newVars = Vars HM.empty

getVar :: Vars -> String -> AST.Operand
getVar (Vars v) s =
  case HM.lookup s v of
    Just o -> o
    Nothing -> error $ "Variable " ++ s ++ " not found"

-- | Compile a program.
compileProgramToFile :: FilePath -> CompilerConfig -> Program -> IO ()
compileProgramToFile path cfg ast =
  withContext $ \ctx ->
    withModuleFromAST ctx (genModule ast) $ \modir ->
      emit (cfgFormat cfg) path modir

emit :: OutputFormat -> FilePath -> Module -> IO ()
emit Assembly path modir =
  removePathForcibly path
    >> writeLLVMAssemblyToFile (File path) modir
emit NativeObject path modir =
  removePathForcibly path
    >> withHostTargetMachineDefault
      ( \target ->
          writeObjectToFile target (File path) modir
      )

assign :: MonadState Vars e => String -> AST.Operand -> e ()
assign name addr = modify $
  \e -> e {operands = HM.insert name addr (operands e)}

genModule :: Program -> AST.Module
genModule (Program defs) =
  buildModule "__main_module" $ traverse_ genDef defs

genDef :: MonadModuleBuilder m => Definition -> m AST.Operand
-- main special case
genDef (DFn (Ident "main") [] TInt32 body) =
  function (AST.mkName "__koa_main") [] LLVMType.i32 $ genBody body
genDef (DFn (Ident "main") [] TEmpty body) =
  function (AST.mkName "__koa_main") [] LLVMType.i32 $ \ops ->
    genBody body ops <* ret (int32 0)
genDef (DFn (Ident "main") [] _ _) =
  error "`main` can only return empty or i32"
genDef (DFn (Ident "main") _ _ _) =
  error "`main` must have no arguments"
-- normal functions
genDef (DFn (Ident name) args rety body) =
  function (AST.mkName name) (genArgs args) (llvmType rety) $ genBody body

genArgs :: [(Ident, Type)] -> [(LLVMType.Type, ParameterName)]
genArgs args = genArg <$> args
  where
    genArg (Ident pname, ptype) =
      (llvmType ptype, ParameterName $ fromString pname)

genBody :: MonadIRBuilder m => [Stmt] -> [AST.Operand] -> m ()
genBody stmts [] =
  do
    _ <- block `named` "entry"
    traverse_ (genStmt newVars) stmts
genBody _ _ = error "unimplemented body without expression"

genStmt :: (MonadIRBuilder m) => Vars -> Stmt -> m Vars
genStmt var (SLet pat t expr) =
  do
    vars <- genStmtLet var pat t expr
    pure $ execState vars var
genStmt var (SReturn (EConst CEmpty)) = retVoid >> pure var
genStmt _ (SReturn _) = error "unimplemented return statement"
genStmt _ (SExpr _) = error "unimplemented expression statement"
genStmt _ (SBreak _) = error "unimplemented break statement"

genStmtLet ::
  (MonadIRBuilder m, MonadState Vars e) =>
  Vars ->
  Pattern ->
  Type ->
  Expr ->
  m (e ())
genStmtLet var PWildcard _ expr =
  do
    _ <- genExpr var expr
    pure $ pure ()
genStmtLet var (PIdent (Ident name)) t expr =
  genExpr var expr >>= allocate name t
genStmtLet var (PMutIdent (Ident name)) t expr =
  genExpr var expr >>= allocate name t

allocate ::
  (MonadIRBuilder m, MonadState Vars e) =>
  String ->
  Type ->
  AST.Operand ->
  m (e ())
allocate name t val =
  do
    addr <- alloca (llvmType t) (Just val) 0
    store addr 0 val
    pure $ assign name addr

genExpr :: MonadIRBuilder m => Vars -> Expr -> m AST.Operand
-- literal
genExpr _ (EConst lit) = pure $ genConst lit
-- unary op
genExpr var (EUnop op e) = genExpr var e >>= genUnop op
-- binary op
genExpr var (EBinop op left right) =
  do
    left' <- genExpr var left
    right' <- genExpr var right
    genBinop op left' right'
-- other
genExpr var (EVar t (Ident name)) = genIdent var (Ident name, t)
genExpr _ EBlock {} = error "unimplemented genExpr.EBlock"
genExpr _ EIf {} = error "unimplemented genExpr.EIf"
genExpr _ ELoop {} = error "unimplemented genExpr.ELoop"
genExpr _ ECall {} = error "unimplemented genExpr.ECall"
genExpr _ EAssign {} = error "unimplemented genExpr.EAssign"

genUnop :: MonadIRBuilder m => Unop -> AST.Operand -> m AST.Operand
genUnop ONeg = sub (int32 0)
genUnop _ = error "unimplemented genUnop'"

genBinop ::
  MonadIRBuilder m =>
  Binop ->
  AST.Operand ->
  AST.Operand ->
  m AST.Operand
genBinop OAdd = add
genBinop OSub = sub
genBinop OMul = mul
genBinop ODiv = sdiv
genBinop OEquals = icmp IPred.EQ
genBinop ONotEquals = icmp IPred.NE
genBinop OLessThan = icmp IPred.SLT
genBinop OGreaterThan = icmp IPred.SGT
genBinop OLessThanEq = icmp IPred.SLE
genBinop OGreaterThanEq = icmp IPred.SGE

genIdent :: MonadIRBuilder m => Vars -> (Ident, Type) -> m AST.Operand
genIdent var (Ident name, _) = load (getVar var name) 0

genConst :: Constant -> AST.Operand
genConst (CInt32 n) = int32 $ fromIntegral n
genConst (CFloat64 n) = double n
genConst (CBool True) = bit 1
genConst (CBool False) = bit 0
genConst CEmpty = error "can't generate empty literal"

llvmType :: Type -> LLVMType.Type
llvmType TInt32 = LLVMType.i32
llvmType TFloat64 = LLVMType.double
llvmType TEmpty = LLVMType.void
llvmType TBool = LLVMType.i1
