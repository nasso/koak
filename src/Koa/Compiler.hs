{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Koa.Compiler
  ( CompilerConfig (..),
    OutputFormat (..),
    compileProgramToFile,
  )
where

import Control.Monad.Reader
import Data.ByteString.Short (ShortByteString)
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.String (IsString (fromString))
import Koa.Syntax.MIR
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as ASTC
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
  runReaderT
    (buildModuleT "__main_module" $ runModgen $ genRecDefs defs)
    newScope

mangled :: Ident -> AST.Name
mangled (Ident name) = AST.mkName $ "__koa_" <> name

patName :: IsString p => Pattern -> p
patName (PIdent (Ident n)) = fromString n
patName (PMutIdent (Ident n)) = fromString n
patName PWildcard = fromString "unused_parameter"

data Scope = Scope
  { scpVars :: HashMap String AST.Operand,
    scpBreakDest :: Maybe AST.Name
  }

newScope :: Scope
newScope =
  Scope
    { scpVars = HM.empty,
      scpBreakDest = Nothing
    }

getVar :: Ident -> Scope -> AST.Operand
getVar (Ident i) v =
  case HM.lookup i (scpVars v) of
    Just o -> o
    Nothing -> error $ "Variable " ++ i ++ " not found"

setVar :: Ident -> AST.Operand -> Scope -> Scope
setVar (Ident i) val v = v {scpVars = HM.insert i val (scpVars v)}

setAllVars :: Foldable f => f (Ident, AST.Operand) -> Scope -> Scope
setAllVars vars v = foldl (\v' (i, o) -> setVar i o v') v vars

setBreakDest :: AST.Name -> Scope -> Scope
setBreakDest dest v = v {scpBreakDest = Just dest}

newtype Modgen a = Modgen
  { runModgen ::
      ModuleBuilderT
        ( ReaderT
            Scope
            IO
        )
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadFix,
      MonadFail,
      MonadReader Scope,
      MonadModuleBuilder
    )

genRecDefs :: [Definition] -> Modgen ()
genRecDefs defs =
  local (setAllVars bindings) $ traverse_ genDef defs
  where
    names = [n | DFn n _ _ _ <- defs]
    bindings = zip names functions
    functions = refTo <$> defs

refTo :: Definition -> AST.Operand
refTo (DFn name args rety _) =
  AST.ConstantOperand $ ASTC.GlobalReference funty (mangled name)
  where
    funty = LLVMType.ptr $ LLVMType.FunctionType (llvmType rety) argtys False
    argtys = [llvmType ty | TBinding _ ty <- args]

genDef :: Definition -> Modgen AST.Operand
-- main special case
genDef (DFn name@(Ident "main") [] rety body) =
  function (mangled name) [] LLVMType.i32 $ const $ runCodegen $ bodyFor rety
  where
    bodyFor TInt32 = genBody [] body
    bodyFor TEmpty = genBody [] body <* ret (int32 0)
    bodyFor _ = error "`main` can only return empty or i32"
genDef (DFn (Ident "main") _ _ _) = error "`main` must take no parameter"
-- normal functions
genDef (DFn name args rety body) =
  function (mangled name) (genArg <$> args) (llvmType rety) $
    \ops -> runCodegen $ genBody (argvals ops) body
  where
    argvals ops = zipWith bindOp ops args
    bindOp op (TBinding pat ty) = (pat, ty, op)
    genArg (TBinding pat t) = (llvmType t, ParameterName $ patName pat)

newtype Codegen a = Codegen
  { runCodegen :: IRBuilderT Modgen a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadFix,
      MonadFail,
      MonadReader Scope,
      MonadIRBuilder,
      MonadModuleBuilder
    )

mkTerminator :: Codegen () -> Codegen ()
mkTerminator t =
  do
    blockTerminated <- hasTerminator
    unless blockTerminated t

return' :: Maybe AST.Operand -> Codegen ()
return' Nothing = mkTerminator retVoid
return' (Just v) = mkTerminator (ret v)

br' :: AST.Name -> Codegen ()
br' = mkTerminator . br

genBody :: [(Pattern, Type, AST.Operand)] -> [Stmt] -> Codegen ()
genBody args stmts =
  block `named` "entry" >> genAllVarDefs args (genAllStmts stmts $ pure ())

genAllStmts :: [Stmt] -> Codegen a -> Codegen a
genAllStmts ss k = foldr genStmt k ss

genStmt :: Stmt -> Codegen a -> Codegen a
genStmt (SLet pat ty expr) k =
  do
    expr' <- genExpr expr
    case expr' of
      Nothing -> k
      Just op -> genVarDef pat ty op k
genStmt (SReturn e) k = (return' =<< genExpr e) >> k
genStmt (SExpr e) k = genExpr e >> k
genStmt (SBreak e) k =
  do
    Just d <- asks scpBreakDest
    Nothing <- genExpr e
    br' d >> k

genVarDef :: Pattern -> Type -> AST.Operand -> Codegen a -> Codegen a
genVarDef PWildcard _ _ k = k
genVarDef _ TEmpty _ k = k
genVarDef (PIdent name) ty op k = allocate name ty op k
genVarDef (PMutIdent name) ty op k = allocate name ty op k

genAllVarDefs :: [(Pattern, Type, AST.Operand)] -> Codegen a -> Codegen a
genAllVarDefs vars k = foldr def' k vars
  where
    def' (pat, t, op) k' = genVarDef pat t op k'

allocate :: Ident -> Type -> AST.Operand -> Codegen a -> Codegen a
allocate name t val k =
  do
    addr <- alloca (llvmType t) Nothing 0
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
genExpr (EVar t name) = genIdent name t
genExpr (EBlock bl) = genInlineBlock bl
genExpr (EIf cond then' else') =
  mdo
    Just cond' <- genExpr cond
    condBr cond' thenBlock elseBlock
    (thenBlock, thenVal) <- genBlock "then" then' <* br' mergeBlock
    (elseBlock, elseVal) <- genBlock "else" else' <* br' mergeBlock
    mergeBlock <- block `named` "merge"
    case (thenVal, elseVal) of
      (Just thenVal', Just elseVal') ->
        Just <$> phi [(thenVal', thenBlock), (elseVal', elseBlock)]
      _ -> pure Nothing
genExpr (ELoop bl) =
  mdo
    (loopStart, loopVal) <- local (setBreakDest exitBlock) $ genBlock "loop" bl
    br' loopStart
    exitBlock <- block `named` "loop_exit"
    case loopVal of
      Nothing -> pure Nothing
      Just _ -> error "unimplemented loop with non-void break"
genExpr (ECall name args) =
  do
    f <- asks $ getVar name
    args' <- catMaybes <$> mapM genExpr args
    Just <$> call f [(a, []) | a <- args']
genExpr (EAssign name e) = genAssign name e

genBlock :: ShortByteString -> Block -> Codegen (AST.Name, Maybe AST.Operand)
genBlock label bl =
  mdo
    br' name
    name <- block `named` label
    res <- genInlineBlock bl
    pure (name, res)

genInlineBlock :: Block -> Codegen (Maybe AST.Operand)
genInlineBlock (Block stmts e) = genAllStmts stmts (genExpr e)

genUnop :: Unop -> AST.Operand -> Codegen AST.Operand
genUnop ONeg = sub (int32 0)
genUnop ONot = icmp IPred.EQ (bit 0)

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

genIdent :: Ident -> Type -> Codegen (Maybe AST.Operand)
genIdent _ TEmpty = pure Nothing
genIdent name _ =
  do
    v <- asks $ getVar name
    Just <$> load v 0

genAssign :: Ident -> Expr -> Codegen (Maybe AST.Operand)
genAssign name e =
  do
    e' <- genExpr e
    forM_ e' $ \e'' ->
      do
        v <- asks $ getVar name
        store v 0 e''
    pure e'

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
