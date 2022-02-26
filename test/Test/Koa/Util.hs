module Test.Koa.Util where

import Koa.Syntax.HIR

class ExprLike a where
  lit :: Type -> Literal -> a
  var :: Type -> String -> a

  litEmpty :: a
  litEmpty = lit TEmpty LEmpty

  litBool :: Bool -> a
  litBool = lit TBool . LBool

  litI32 :: Integer -> a
  litI32 = lit TInt32 . LInt

  litF64 :: Double -> a
  litF64 = lit TFloat64 . LFloat

  varEmpty :: String -> a
  varEmpty = var TEmpty

  varBool :: String -> a
  varBool = var TBool

  varI32 :: String -> a
  varI32 = var TInt32

  varF64 :: String -> a
  varF64 = var TFloat64

instance ExprLike Expr where
  lit _ = Expr . ELit
  var _ = Expr . EIdent . Ident

instance ExprLike ExprT where
  lit t l = ExprT (ELit l, t)
  var t n = ExprT (EIdent $ Ident n, t)
