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
compileProgramToFile :: FilePath -> CompilerConfig -> ProgramT -> IO ()
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

genModule :: ProgramT -> AST.Module
genModule (Program defs) =
  buildModule "__main_module" $
    traverse_
      ( \def -> case def of
          (DFn (Ident "main") [] rety body) -> genMainDef rety body
          (DFn (Ident "main") _ _ _) -> error "`main` must have no arguments"
          _ -> genDef def
      )
      defs

genMainDef :: MonadModuleBuilder m => Type -> BlockF ExprT -> m AST.Operand
genMainDef TInt32 body =
  function (AST.mkName "__koa_main") [] LLVMType.i32 $ genBody body
genMainDef TEmpty body =
  function (AST.mkName "__koa_main") [] LLVMType.i32 $ \ops ->
    genBody body ops <* ret (int32 0)
genMainDef _ _ = error "`main` can only return empty or i32"

genDef :: MonadModuleBuilder m => DefinitionT -> m AST.Operand
genDef (DFn (Ident name) args rety body) =
  function (AST.mkName name) (arg <$> args) (llvmType rety) $ genBody body
  where
    arg = error "unimplemented genDef.arg"

genBody :: MonadIRBuilder m => BlockT -> [AST.Operand] -> m ()
genBody (BExpr [] (Just expr@(ExprT (_, TEmpty)))) [] =
  do
    _ <- block `named` "entry"
    _ <- genExpr expr
    retVoid
genBody (BExpr [] (Just expr)) [] =
  do
    _ <- block `named` "entry"
    res <- genExpr expr
    ret res
genBody _ _ = error "unimplemented genBody for statements"

genExpr :: MonadIRBuilder m => ExprT -> m AST.Operand
-- literal
genExpr (ExprT (ELit lit, _)) = pure $ genLiteral lit
-- unary op
genExpr (ExprT (EUnop op e, _)) =
  do
    e' <- genExpr e
    genUnop op e'
-- binary op
genExpr (ExprT (EBinop op left right, _)) =
  do
    left' <- genExpr left
    right' <- genExpr right
    genBinop op left' right'
-- other
genExpr (ExprT (EIdent (Ident _), _)) = error "unimplemented genExpr.Ident"
genExpr (ExprT (EBlock _, _)) = error "unimplemented genExpr.EBlock"
genExpr (ExprT (EIf _ _ _, _)) = error "unimplemented genExpr.EIf"
genExpr (ExprT (EWhile _ _, _)) = error "unimplemented genExpr.EWhile"
genExpr (ExprT (EFor _ _ _ _, _)) = error "unimplemented genExpr.EFor"
genExpr (ExprT (ECall (Ident _) _, _)) = error "unimplemented genExpr.ECall"
genExpr (ExprT (EAssign (Ident _) _, _)) = error "unimplemented genExpr.EAssign"

genUnop :: MonadIRBuilder m => Unop -> AST.Operand -> m AST.Operand
genUnop ONeg = sub (int32 0)
genUnop _ = error "unimplemented genUnop'"

genBinop :: MonadIRBuilder m => Binop -> AST.Operand -> AST.Operand -> m AST.Operand
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

genLiteral :: Literal -> AST.Operand
genLiteral (LInt n) = int32 n
genLiteral (LFloat n) = double n
genLiteral (LBool True) = bit 1
genLiteral (LBool False) = bit 0
genLiteral LEmpty = error "can't generate empty literal"

llvmType :: Type -> LLVMType.Type
llvmType TInt32 = LLVMType.i32
llvmType TFloat64 = LLVMType.double
llvmType TEmpty = LLVMType.void
llvmType TBool = LLVMType.i1
