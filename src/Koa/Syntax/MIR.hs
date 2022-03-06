module Koa.Syntax.MIR where

import Data.Int

-- | MIR for a program.
newtype Program = Program [Definition] deriving (Eq, Show)

-- | An identifier (for a variable, function, etc...).
newtype Ident = Ident String deriving (Eq, Show)

-- | A definition.
data Definition
  = DFn Ident [TBinding] Type [Stmt]
  deriving (Eq, Show)

-- | An arbitrary identifier binding (e.g. a function parameter).
data TBinding = TBinding Pattern Type deriving (Eq, Show)

-- | A statement (@let@, @return@, or an expression with side-effects).
data Stmt
  = SExpr Expr
  | SReturn Expr
  | SBreak Expr
  | SLet Pattern Type Expr
  deriving (Eq, Show)

-- | A pattern (used by @let@ statements).
data Pattern
  = PWildcard
  | PIdent Ident
  | PMutIdent Ident
  deriving (Eq, Show)

-- | MIR for an expression.
data Expr
  = EVar Type Ident
  | EBlock Block
  | EIf Expr Block Block
  | ELoop Block
  | ECall Ident [Expr]
  | EAssign Ident Expr
  | EBinop Type Binop Expr Expr
  | EUnop Unop Expr
  | EConst Constant
  deriving (Eq, Show)

-- | A list of statements, evaluating to a final expression.
data Block = Block [Stmt] Expr deriving (Eq, Show)

-- | A constant value (@12@, @0.2@, @()@, etc...).
data Constant
  = CInt32 Int32
  | CFloat64 Double
  | CBool Bool
  | CEmpty
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
