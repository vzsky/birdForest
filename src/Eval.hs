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

appCont :: Int -> Int -> Expr -> Expr
appCont = apply ... f where 
  f a c (Var n) | (c <= n) = Var (n+a)
  f a c e                  = e

joinCont :: Int -> Expr -> Expr
joinCont n (Cont l e) = (Cont (n+l) e)
joinCont n e          = (Cont n     e)

interp :: Expr -> Expr
interp = intrp 0 
  where
    intrp l I = joinCont 1 (Var l)
    intrp l K = joinCont 2 (Var l)
    intrp l S = joinCont 3 (App (App (Var l) (Var (l+2))) (App (Var (l+1)) (Var (l+2))))

    intrp l (App f x) = case intrp l f of 
      Cont 1 e -> intrp l $ (substitute l x e)
      Cont n e -> intrp l $ joinCont (n-1) $ (substitute l (appCont (n-1) l x) e)
      e        -> (App e $ intrp l x)

    intrp l (Cont lv e) = (joinCont lv (intrp (l+lv) e))
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
sugarize (Cont n e) 
  | n == 1 && e == (Var 0)                                              = I
  | n == 2 && e == (Var 0)                                              = K
  | n == 3 && e == (list2app [(Var 0), (Var 2), (App (Var 1) (Var 2))]) = S
  | n == 3 && e == (App (Var 0) (App (Var 1) (Var 2)))                  = B
  | n == 3 && e == (App (App (Var 0) (Var 2)) (Var 1))                  = C

sugarize (Cont 1 e) = joinCont 1 $ ((addVar 1) . sugarize . (addVar (-1))) e
sugarize (Cont n e) = joinCont 1 $ ((addVar 1) . sugarize . (addVar (-1))) (Cont (n-1) e)
sugarize (App e1 e2) = App (sugarize e1) (sugarize e2)
sugarize e = e

eval :: Expr -> Expr
eval = interp . desugarize
