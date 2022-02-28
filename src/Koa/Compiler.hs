{-# LANGUAGE OverloadedStrings #-}

module Koa.Compiler
  ( CompilerConfig (..),
    OutputFormat (..),
    compileProgramToFile,
  )
where

import Data.Foldable
import qualified Koa.Syntax.MIR as MIR
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
compileProgramToFile :: FilePath -> CompilerConfig -> MIR.Program -> IO ()
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

genModule :: MIR.Program -> AST.Module
genModule (MIR.Program defs) =
  buildModule "__main_module" $ traverse_ genDef defs

genDef :: MonadModuleBuilder m => MIR.Definition -> m AST.Operand
-- main special case
genDef (MIR.DFn (MIR.Ident "main") [] MIR.TInt32 body) =
  function (AST.mkName "__koa_main") [] LLVMType.i32 $ genBody body
genDef (MIR.DFn (MIR.Ident "main") [] MIR.TEmpty body) =
  function (AST.mkName "__koa_main") [] LLVMType.i32 $ \ops ->
    genBody body ops <* ret (int32 0)
genDef (MIR.DFn (MIR.Ident "main") [] _ _) =
  error "`main` can only return empty or i32"
genDef (MIR.DFn (MIR.Ident "main") _ _ _) =
  error "`main` must have no arguments"
-- normal functions
genDef (MIR.DFn (MIR.Ident name) args rety body) =
  function (AST.mkName name) (arg <$> args) (llvmType rety) $ genBody body
  where
    arg = error "unimplemented genDef.arg"

genBody :: MonadIRBuilder m => [MIR.Stmt] -> [AST.Operand] -> m ()
genBody [MIR.SReturn expr@(MIR.EConst MIR.CEmpty)] [] =
  do
    _ <- block `named` "entry"
    _ <- genExpr expr
    retVoid
genBody [MIR.SReturn expr] [] =
  do
    _ <- block `named` "entry"
    res <- genExpr expr
    ret res
genBody _ _ = error "unimplemented genBody for statements"

genExpr :: MonadIRBuilder m => MIR.Expr -> m AST.Operand
-- literal
genExpr (MIR.EConst lit) = pure $ genConst lit
-- unary op
genExpr (MIR.EUnop op e) =
  do
    e' <- genExpr e
    genUnop op e'
-- binary op
genExpr (MIR.EBinop op left right) =
  do
    left' <- genExpr left
    right' <- genExpr right
    genBinop op left' right'
-- other
genExpr MIR.EVar {} = error "unimplemented genExpr.EVar"
genExpr MIR.EBlock {} = error "unimplemented genExpr.EBlock"
genExpr MIR.EIf {} = error "unimplemented genExpr.EIf"
genExpr MIR.ELoop {} = error "unimplemented genExpr.ELoop"
genExpr MIR.ECall {} = error "unimplemented genExpr.ECall"
genExpr MIR.EAssign {} = error "unimplemented genExpr.EAssign"

genUnop :: MonadIRBuilder m => MIR.Unop -> AST.Operand -> m AST.Operand
genUnop MIR.ONeg = sub (int32 0)
genUnop _ = error "unimplemented genUnop'"

genBinop ::
  MonadIRBuilder m =>
  MIR.Binop ->
  AST.Operand ->
  AST.Operand ->
  m AST.Operand
genBinop MIR.OAdd = add
genBinop MIR.OSub = sub
genBinop MIR.OMul = mul
genBinop MIR.ODiv = sdiv
genBinop MIR.OEquals = icmp IPred.EQ
genBinop MIR.ONotEquals = icmp IPred.NE
genBinop MIR.OLessThan = icmp IPred.SLT
genBinop MIR.OGreaterThan = icmp IPred.SGT
genBinop MIR.OLessThanEq = icmp IPred.SLE
genBinop MIR.OGreaterThanEq = icmp IPred.SGE

genConst :: MIR.Constant -> AST.Operand
genConst (MIR.CInt32 n) = int32 $ fromIntegral n
genConst (MIR.CFloat64 n) = double n
genConst (MIR.CBool True) = bit 1
genConst (MIR.CBool False) = bit 0
genConst MIR.CEmpty = error "can't generate empty literal"

llvmType :: MIR.Type -> LLVMType.Type
llvmType MIR.TInt32 = LLVMType.i32
llvmType MIR.TFloat64 = LLVMType.double
llvmType MIR.TEmpty = LLVMType.void
llvmType MIR.TBool = LLVMType.i1
