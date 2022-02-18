{-# LANGUAGE OverloadedStrings #-}

module Koa.Compiler
  ( CompilerConfig (..),
    CompilationTarget (..),
    compileProgram,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import Koa.Syntax
import qualified LLVM.AST as LLVMAST
import qualified LLVM.AST.Type as LLVMType
import LLVM.IRBuilder

-- | Configuration for the compiler.
newtype CompilerConfig = CompilerConfig
  { cfgTarget :: CompilationTarget
  }
  deriving (Show, Eq)

-- | A compilation target.
data CompilationTarget = Assembly | Bitcode | NativeObject deriving (Show, Eq)

-- | Compile a program.
compileProgram :: CompilerConfig -> ProgramT -> IO ByteString
compileProgram _ ast = pure $ BS.pack $ show $ genModule ast

genModule :: ProgramT -> LLVMAST.Module
genModule (Program defs) = buildModule "__main_module" $ traverse_ genDef defs

genDef :: MonadModuleBuilder m => DefinitionT -> m LLVMAST.Operand
genDef (DFn (Ident name) args rety body) =
  function (LLVMAST.mkName name) (arg <$> args) (llvmType rety) $ genBody body
  where
    arg = error "unimplemented genDef.arg"

genBody :: MonadIRBuilder m => BlockT -> [LLVMAST.Operand] -> m ()
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
