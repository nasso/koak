module Syntax (module Syntax) where

-- | An identifier.
newtype Ident = Ident String deriving (Show, Eq)

-- | A type.
data Type = Int | Double | Void deriving (Show, Eq)

-- | Abstract syntax tree for a program in the Kaleidoscope language.
newtype Ast = Ast [Statement] deriving (Show, Eq)

-- | Abstract syntax tree for a statement.
data Statement
  = -- | A function definition.
    Def
      { -- | The name of the function.
        defName :: Ident,
        -- | The arguments of the function.
        defArgs :: [Argument],
        -- | The return type of the function.
        defType :: Type,
        -- | The body of the function.
        defBody :: ExpressionBlock
      }
  | -- | A list of expressions.
    Block ExpressionBlock
  deriving (Show, Eq)

-- | Abstract syntax tree for an argument.
data Argument = Argument Ident Type deriving (Show, Eq)

-- | Abstract syntax tree for an expression block.
data ExpressionBlock
  = -- | A list of expressions.
    Multi [Expression]
  | -- | For loop.
    For
      { -- | The initialisation expression.
        forInit :: Expression,
        -- | The condition expression.
        forCond :: Expression,
        -- | The update expression.
        forUpdate :: Expression,
        -- | The body of the loop.
        forBody :: ExpressionBlock
      }
  | -- | If statement.
    If
      { -- | The condition expression.
        ifCond :: Expression,
        -- | The body of the if statement.
        ifBody :: ExpressionBlock,
        -- | The body of the else statement.
        ifElse :: Maybe ExpressionBlock
      }
  | -- | While loop.
    While
      { -- | The condition expression.
        whileCond :: Expression,
        -- | The body of the loop.
        whileBody :: ExpressionBlock
      }
  deriving (Show, Eq)

-- | Abstract syntax tree for an expression.
data Expression
  = -- | A unary expression.
    Unary UnaryOp Expression
  | -- | A binary expression.
    Binary BinaryOp Expression Expression
  | -- | A call expression.
    Call Primary [Expression]
  | -- | A primary expression.
    Primary Primary
  deriving (Show, Eq)

-- | Abstract syntax tree for a primary expression.
data Primary
  = -- | A variable reference.
    Var Ident
  | -- | A literal.
    Literal Literal
  | -- | An expression block in parentheses.
    Paren ExpressionBlock
  deriving (Show, Eq)

-- | Abstract syntax tree for a literal.
data Literal
  = -- | A literal integer.
    IntLiteral Integer
  | -- | A literal double.
    DoubleLiteral Double
  deriving (Show, Eq)

-- | TODO
data UnaryOp = UnaryOp deriving (Show, Eq)

-- | TODO
data BinaryOp = BinaryOp deriving (Show, Eq)
