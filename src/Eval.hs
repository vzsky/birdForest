module Eval
  ( interp ) where

import Expr

(...) = (.)(.)(.)

substitute :: Expr -> Expr -> Expr
substitute = subst 0 
  where 
    subst l x (App e1 e2)        = App (subst l x e1) (subst l x e2)
    subst l x (Lamd n e)         = Lamd n (subst (l+n) (add n x) e)
    subst l x (Var m) | (l == m) = x
                          | (l < m)  = (Var (m-1))
    subst l x e                  = e

add :: Int -> Expr -> Expr
add n (App e1 e2) = App (add n e1) (add n e2)
add n (Var m)     = Var (m+n)
add n e           = e

joinLamd :: Int -> Expr -> Expr
joinLamd 0 e          = e
joinLamd n (Lamd l e) = (Lamd (n+l) e)
joinLamd n e          = (Lamd n     e)

interp :: Expr -> Expr
interp K = Lamd 2 (Var 1)
interp S = Lamd 3 (App (App (Var 2) (Var 0)) (App (Var 1) (Var 0)))

interp (App f x) = case interp f of 
  Lamd n e -> interp $ substitute x (joinLamd (n-1) e)
  e        -> (App e $ interp x)

interp (Lamd lv e) = (joinLamd lv (interp e))
interp x = x
