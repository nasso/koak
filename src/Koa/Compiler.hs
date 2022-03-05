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

-- | Compile a program.
compileProgramToFile :: FilePath -> CompilerConfig -> Program -> IO ()
compileProgramToFile path cfg ast =
  withContext $ \ctx ->
    do
      module' <- genModule ast
      withModuleFromAST ctx module' $ \modir ->
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

genModule :: Program -> IO AST.Module
genModule (Program defs) =
  buildModuleT "__main_module" $ traverse_ genDef defs

newtype Vars = Vars
  { varOperands :: HashMap String AST.Operand
  }
  deriving (Eq, Show)

newVars :: Vars
newVars = Vars HM.empty

getVar :: Ident -> Vars -> AST.Operand
getVar (Ident i) (Vars v) =
  case HM.lookup i v of
    Just o -> o
    Nothing -> error $ "Variable " ++ i ++ " not found"

setVar :: Ident -> AST.Operand -> Vars -> Vars
setVar (Ident i) val v = v {varOperands = HM.insert i val (varOperands v)}

newtype Codegen a = Codegen
  { runCodegen :: ReaderT Vars (IRBuilderT (ModuleBuilderT IO)) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadReader Vars,
      MonadIRBuilder,
      MonadModuleBuilder
    )

genDef :: Definition -> ModuleBuilderT IO AST.Operand
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
genStmt (SReturn e) k = (maybe retVoid ret =<< genExpr e) >> k
genStmt (SExpr e) k = genExpr e >> k
genStmt (SBreak _) _ = error "unimplemented break statement"

genStmtLet :: Pattern -> Type -> Expr -> Codegen a -> Codegen a
genStmtLet PWildcard _ e k = genExpr e >> k
genStmtLet _ TEmpty e k = genExpr e >> k
genStmtLet (PIdent name) t expr k =
  do
    Just expr' <- genExpr expr
    allocate name t expr' k
genStmtLet (PMutIdent name) t expr k =
  do
    Just expr' <- genExpr expr
    allocate name t expr' k

allocate :: Ident -> Type -> AST.Operand -> Codegen a -> Codegen a
allocate name t val k =
  do
    addr <- alloca (llvmType t) (Just val) 0
    store addr 0 val
    local (setVar name addr) k

genExpr :: Expr -> Codegen (Maybe AST.Operand)
-- literal
genExpr (EConst c) = pure $ llvmConst c
-- unary op
genExpr (EUnop operator e) =
  do
    Just e' <- genExpr e
    Just <$> genUnop operator e'
-- binary op
genExpr (EBinop op left right) =
  do
    Just left' <- genExpr left
    Just right' <- genExpr right
    Just <$> genBinop op left' right'
-- other
genExpr (EVar t (Ident name)) = genIdent (Ident name, t)
genExpr EBlock {} = error "unimplemented genExpr.EBlock"
genExpr EIf {} = error "unimplemented genExpr.EIf"
genExpr ELoop {} = error "unimplemented genExpr.ELoop"
genExpr ECall {} = error "unimplemented genExpr.ECall"
genExpr (EAssign (Ident name) e) = genAssign (Ident name, e)

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

genIdent :: (Ident, Type) -> Codegen (Maybe AST.Operand)
genIdent (_, TEmpty) = pure Nothing
genIdent (name, _) =
  do
    v <- asks $ getVar name
    Just <$> load v 0

genAssign :: (Ident, Expr) -> Codegen (Maybe AST.Operand)
genAssign (name, e) =
  do
    Just e' <- genExpr e
    v <- asks $ getVar name
    store v 0 e'
    pure Nothing

llvmConst :: Constant -> Maybe AST.Operand
llvmConst (CInt32 n) = Just $ int32 $ fromIntegral n
llvmConst (CFloat64 n) = Just $ double n
llvmConst (CBool True) = Just $ bit 1
llvmConst (CBool False) = Just $ bit 0
llvmConst CEmpty = Nothing

llvmType :: Type -> LLVMType.Type
llvmType TInt32 = LLVMType.i32
llvmType TFloat64 = LLVMType.double
llvmType TEmpty = LLVMType.void
llvmType TBool = LLVMType.i1
