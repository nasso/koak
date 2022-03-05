{-# LANGUAGE OverloadedStrings #-}

module Koa.Compiler
  ( CompilerConfig (..),
    OutputFormat (..),
    compileProgramToFile,
  )
where

import Data.Foldable
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
  function (AST.mkName name) (arg <$> args) (llvmType rety) $ genBody body
  where
    arg = error "unimplemented genDef.arg"

genBody :: MonadIRBuilder m => [Stmt] -> [AST.Operand] -> m ()
genBody [SReturn expr@(EConst CEmpty)] [] =
  do
    _ <- block `named` "entry"
    _ <- genExpr expr
    retVoid
genBody [SReturn expr] [] =
  do
    _ <- block `named` "entry"
    res <- genExpr expr
    ret res
genBody _ _ = error "unimplemented genBody for statements"

genExpr :: MonadIRBuilder m => Expr -> m AST.Operand
-- literal
genExpr (EConst lit) = pure $ genConst lit
-- unary op
genExpr (EUnop op e) =
  do
    e' <- genExpr e
    genUnop op e'
-- binary op
genExpr (EBinop op left right) =
  do
    left' <- genExpr left
    right' <- genExpr right
    genBinop op left' right'
-- other
genExpr EVar {} = error "unimplemented genExpr.EVar"
genExpr EBlock {} = error "unimplemented genExpr.EBlock"
genExpr EIf {} = error "unimplemented genExpr.EIf"
genExpr ELoop {} = error "unimplemented genExpr.ELoop"
genExpr ECall {} = error "unimplemented genExpr.ECall"
genExpr EAssign {} = error "unimplemented genExpr.EAssign"

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
