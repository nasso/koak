{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Koa.Compiler
  ( CompilerConfig (..),
    OutputFormat (..),
    compileProgramToFile,
  )
where

import Control.Monad.Reader
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

getVar :: String -> Vars -> AST.Operand
getVar s (Vars v) =
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

genModule :: Program -> AST.Module
genModule (Program defs) =
  buildModule "__main_module" $ traverse_ genDef defs

newtype Codegen a = Codegen
  { runCodegen :: ReaderT Vars (IRBuilderT ModuleBuilder) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Vars,
      MonadIRBuilder,
      MonadModuleBuilder
    )

assign :: String -> AST.Operand -> Vars -> Vars
assign name addr v = v {operands = HM.insert name addr (operands v)}

genDef :: Definition -> ModuleBuilder AST.Operand
-- main special case
genDef (DFn (Ident "main") [] rety body) =
  function (AST.mkName "__koa_main") [] LLVMType.i32 $
    \ops -> runReaderT (runCodegen $ bodyFor rety ops) newVars
  where
    bodyFor TInt32 ops = genBody body ops
    bodyFor TEmpty ops = genBody body ops <* ret (int32 0)
    bodyFor _ _ = error "`main` can only return empty or i32"
genDef (DFn (Ident "main") _ _ _) = error "`main` must have no arguments"
-- normal functions
genDef (DFn (Ident name) args rety body) =
  function (AST.mkName name) (genArgs args) (llvmType rety) $
    \ops -> runReaderT (runCodegen $ genBody body ops) newVars

genArgs :: [(Ident, Type)] -> [(LLVMType.Type, ParameterName)]
genArgs args = genArg <$> args
  where
    genArg (Ident pname, ptype) =
      (llvmType ptype, ParameterName $ fromString pname)

genBody :: [Stmt] -> [AST.Operand] -> Codegen ()
genBody stmts [] =
  block `named` "entry" >> doStmts stmts
  where
    doStmts [] = pure ()
    doStmts (s : ss) = genStmt s $ doStmts ss
genBody _ _ = error "unimplemented body without expression"

genStmt :: Stmt -> Codegen a -> Codegen a
genStmt (SLet pat t expr) k = genStmtLet pat t expr k
genStmt (SReturn (EConst CEmpty)) k = retVoid >> k
genStmt (SReturn e) k = (ret =<< genExpr e) >> k
genStmt (SExpr _) _ = error "unimplemented expression statement"
genStmt (SBreak _) _ = error "unimplemented break statement"

genStmtLet :: Pattern -> Type -> Expr -> Codegen a -> Codegen a
genStmtLet PWildcard _ expr k = genExpr expr >> k
genStmtLet (PIdent (Ident name)) t expr k =
  do
    expr' <- genExpr expr
    allocate name t expr' k
genStmtLet (PMutIdent (Ident name)) t expr k =
  do
    expr' <- genExpr expr
    allocate name t expr' k

allocate :: String -> Type -> AST.Operand -> Codegen a -> Codegen a
allocate name t val k =
  do
    addr <- alloca (llvmType t) (Just val) 0
    store addr 0 val
    local (assign name addr) k

genExpr :: Expr -> Codegen AST.Operand
-- literal
genExpr (EConst lit) = pure $ genConst lit
-- unary op
genExpr (EUnop op e) = genExpr e >>= genUnop op
-- binary op
genExpr (EBinop op left right) =
  do
    left' <- genExpr left
    right' <- genExpr right
    genBinop op left' right'
-- other
genExpr (EVar t (Ident name)) = genIdent (Ident name, t)
genExpr EBlock {} = error "unimplemented genExpr.EBlock"
genExpr EIf {} = error "unimplemented genExpr.EIf"
genExpr ELoop {} = error "unimplemented genExpr.ELoop"
genExpr ECall {} = error "unimplemented genExpr.ECall"
genExpr EAssign {} = error "unimplemented genExpr.EAssign"

genUnop :: Unop -> AST.Operand -> Codegen AST.Operand
genUnop ONeg = sub (int32 0)
genUnop _ = error "unimplemented genUnop'"

genBinop :: Binop -> AST.Operand -> AST.Operand -> Codegen AST.Operand
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

genIdent :: (Ident, Type) -> Codegen AST.Operand
genIdent (Ident name, _) =
  do
    v <- asks $ getVar name
    load v 0

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
