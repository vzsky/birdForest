module Eval
  ( interp
  , substitute ) where

import Expr

substitute :: Expr -> Expr -> Expr
substitute = subst 0 where 
  subst l x (App e1 e2)        = App (subst l x e1) (subst l x e2)
  subst l x (Lamd n e)         = Lamd n (subst (l+n) (add n x) e)
  subst l x (Var m) | (l == m) = x
                    | (l < m)  = (Var (m-1))
  subst l x e                  = e

add :: Int -> Expr -> Expr
add n (App e1 e2) = App (add n e1) (add n e2)
add n (Var m)     = Var (m+n)
add n e           = e

interp :: Expr -> Result
interp K            = Right $ Lamd 2 (Var 1)
interp S            = Right $ Lamd 3 (App (App (Var 2) (Var 0)) (App (Var 1) (Var 0)))
interp (App f x)    = do 
  func <- interp f
  case func of 
    Lamd n e        -> (return . joinLamd (n-1)) e >>= (interp . substitute x)
    e               -> interp x >>= (return . App e)
interp (Lamd lv e)  =  interp e >>= (return . joinLamd lv)
interp e@(Nest es)  =  unnest e >>= interp
interp e = Right e