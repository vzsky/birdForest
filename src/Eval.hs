module Eval
  ( eval
  , interp
  , desugarize
  , sugarize ) where

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

sugarize :: Expr -> Expr
sugarize (Lamd n e) 
  | n == 1 && e == (Var 0)                                              = I
  | n == 2 && e == (Var 0)                                              = K
  | n == 3 && e == (list2app [(Var 0), (Var 2), (App (Var 1) (Var 2))]) = S
  | n == 3 && e == (App (Var 0) (App (Var 1) (Var 2)))                  = B
  | n == 3 && e == (App (App (Var 0) (Var 2)) (Var 1))                  = C

sugarize (Lamd 1 e) = joinLamd 1 $ ((addVar 1) . sugarize . (addVar (-1))) e
sugarize (Lamd n e) = joinLamd 1 $ ((addVar 1) . sugarize . (addVar (-1))) (Lamd (n-1) e)
sugarize (App e1 e2) = App (sugarize e1) (sugarize e2)
sugarize e = e

eval :: Expr -> Expr
eval = interp . desugarize
