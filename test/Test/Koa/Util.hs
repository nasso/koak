module Test.Koa.Util where

import Koa.Syntax

class FromLit a where
  litEmpty :: a
  litBool :: Bool -> a
  litI32 :: Integer -> a
  litF64 :: Double -> a

instance FromLit Expr where
  litEmpty = Expr $ ELit LEmpty
  litBool = Expr . ELit . LBool
  litI32 = Expr . ELit . LInt
  litF64 = Expr . ELit . LFloat

instance FromLit ExprT where
  litEmpty = ExprT (ELit LEmpty, TEmpty)
  litBool b = ExprT (ELit $ LBool b, TBool)
  litI32 n = ExprT (ELit $ LInt n, TInt32)
  litF64 r = ExprT (ELit $ LFloat r, TFloat64)
