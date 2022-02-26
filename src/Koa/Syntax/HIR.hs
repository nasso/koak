module Koa.Syntax.HIR where

-- | AST for a program.
newtype ProgramF e = Program [DefinitionF e] deriving (Eq, Show)

-- | AST for an untyped program.
type Program = ProgramF Expr

-- | AST for a fully typed program.
type ProgramT = ProgramF ExprT

-- | An identifier (for a variable, function, etc...).
newtype Ident = Ident String deriving (Eq, Show)

-- | A definition.
data DefinitionF e
  = DFn Ident [TBinding] Type (BlockF e)
  deriving (Eq, Show)

-- | AST for an untyped definition.
type Definition = DefinitionF Expr

-- | AST for a fully typed definition.
type DefinitionT = DefinitionF ExprT

-- | An arbitrary identifier binding (e.g. a function parameter).
data TBinding = TBinding Pattern Type deriving (Eq, Show)

-- | AST for an expression.
data ExprF e
  = EIdent Ident
  | EBlock (BlockF e)
  | EIf e (BlockF e) (BlockF e)
  | EWhile e (BlockF e)
  | EFor (StmtF e) e e (BlockF e)
  | ECall Ident [e]
  | EAssign Ident e
  | EBinop Binop e e
  | EUnop Unop e
  | ELit Literal
  deriving (Eq, Show)

-- | AST for an untyped expression.
newtype Expr = Expr (ExprF Expr) deriving (Eq, Show)

-- | AST for a fully typed expression.
newtype ExprT = ExprT (ExprF ExprT, Type) deriving (Eq, Show)

-- | A list of statements, evaluating to a final expression.
data BlockF e = BExpr [StmtF e] (Maybe e) deriving (Eq, Show)

-- | AST for an untyped block expression.
type Block = BlockF Expr

-- | AST for a fully typed block expression.
type BlockT = BlockF ExprT

-- | A statement (@let@, @return@, or an expression with side-effects).
data StmtF e
  = SExpr e
  | SReturn e
  | SLet Pattern (Maybe Type) e
  deriving (Eq, Show)

-- | AST for an untyped statement.
type Stmt = StmtF Expr

-- | AST for a fully typed statement.
type StmtT = StmtF ExprT

-- | A pattern (used by @let@ statements).
data Pattern
  = PWildcard
  | PIdent Ident
  | PMutIdent Ident
  deriving (Eq, Show)

-- | A literal value (@12@, @0.2@, @()@, etc...).
data Literal
  = LInt Integer
  | LFloat Double
  | LBool Bool
  | LEmpty
  deriving (Eq, Show)

-- | A binary operator (@+@, @-@, @*@, etc...).
data Binop
  = OAdd
  | OSub
  | OMul
  | ODiv
  | OEquals
  | ONotEquals
  | OLessThan
  | OGreaterThan
  | OLessThanEq
  | OGreaterThanEq
  deriving (Eq, Show)

-- | An unary operator (@-@, @!@, etc...).
data Unop = ONot | ONeg deriving (Eq, Show)

-- | A type (@i32@, @f64@, @()@, etc...).
data Type
  = TInt32
  | TFloat64
  | TBool
  | TEmpty
  deriving (Eq, Show)
