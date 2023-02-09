module Eval
  ( eval
  , interp
  , desugarize ) where

import Expr

(...) = (.)(.)(.)

substitute :: Int -> Expr -> Expr -> Expr 
substitute = apply ... f where 
  f c x (Var m) | (c == m) = x
                | (c < m)  = Var (m-1)
  f c x e = e

appLamd :: Int -> Int -> Expr -> Expr
appLamd = apply ... f where 
  f a c (Var n) | (c <= n) = Var (n+a)
  f a c e                  = e

joinLamd :: Int -> Expr -> Expr
joinLamd n (Lamd l e) = (Lamd (n+l) e)
joinLamd n e          = (Lamd n     e)

interp :: Expr -> Expr
interp = intrp 0 
  where
    intrp l I = joinLamd 1 (Var l)
    intrp l K = joinLamd 2 (Var l)
    intrp l S = joinLamd 3 (App (App (Var l) (Var (l+2))) (App (Var (l+1)) (Var (l+2))))

    intrp l (App f x) = case intrp l f of 
      Lamd 1 e -> intrp l $ (substitute l x e)
      Lamd n e -> intrp l $ joinLamd (n-1) $ (substitute l (appLamd (n-1) l x) e)
      e        -> (App e $ intrp l x)

    intrp l (Lamd lv e) = (joinLamd lv (intrp (l+lv) e))
    intrp l x = x


desugarize :: Expr -> Expr
desugarize = apply f where 
  f B = list2app [S, App K S, K]
  f C = desugarize $ list2app [S, list2app [B, B, S], list2app [K, K]]
  f e = e

addVar :: Int -> Expr -> Expr
addVar = apply . f where 
  f a (Var n) = Var (n+a)
  f a e       = e

eval :: Expr -> Expr
eval = interp . desugarize
