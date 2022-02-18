{-# LANGUAGE OverloadedStrings #-}

module Koa.Compiler
  ( CompilerConfig (..),
    OutputFormat (..),
    compileProgramToFile,
  )
where

import Data.Foldable
import Koa.Syntax
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as LLVMType
import LLVM.Context
import LLVM.IRBuilder
import LLVM.Module
import LLVM.Target

-- | Configuration for the compiler.
newtype CompilerConfig = CompilerConfig
  { cfgFormat :: OutputFormat
  }
  deriving (Show, Eq)

-- | Output format for the compiled program.
data OutputFormat = Assembly | NativeObject deriving (Show, Eq)

-- | Compile a program.
compileProgramToFile :: FilePath -> CompilerConfig -> ProgramT -> IO ()
compileProgramToFile path cfg ast =
  withContext $ \ctx ->
    withModuleFromAST ctx (genModule ast) $ \modir ->
      emit (cfgFormat cfg) path modir

emit :: OutputFormat -> FilePath -> Module -> IO ()
emit Assembly path modir = writeLLVMAssemblyToFile (File path) modir
emit NativeObject path modir =
  withHostTargetMachineDefault $ \target ->
    writeObjectToFile target (File path) modir

genModule :: ProgramT -> AST.Module
genModule (Program defs) = buildModule "__main_module" $ traverse_ genDef defs

genDef :: MonadModuleBuilder m => DefinitionT -> m AST.Operand
genDef (DFn (Ident name) args rety body) =
  function (AST.mkName name) (arg <$> args) (llvmType rety) $ genBody body
  where
    arg = error "unimplemented genDef.arg"

genBody :: MonadIRBuilder m => BlockT -> [AST.Operand] -> m ()
genBody (BExpr [] (ExprT (ELit LEmpty, TEmpty))) [] =
  do
    _ <- block `named` "entry"
    retVoid
genBody (BExpr [] (ExprT (ELit (LInt n), TInt32))) [] =
  do
    _ <- block `named` "entry"
    ret $ int32 n
genBody (BExpr [] (ExprT (ELit (LFloat n), TFloat64))) [] =
  do
    _ <- block `named` "entry"
    ret $ double n
genBody _ _ = error "unimplemented genBody for statements"

llvmType :: Type -> LLVMType.Type
llvmType TInt32 = LLVMType.i32
llvmType TFloat64 = LLVMType.double
llvmType TEmpty = LLVMType.void
