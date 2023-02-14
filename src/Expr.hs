module Expr 
  ( Expr(..)
  , char2bird
  , allowedChar
  , list2app
  , apply ) where

data Expr 
  = App Expr Expr
  | I | B | C
  | K | S
  | Var Int
  | Lamd Int Expr
  | Gen Int
  deriving (Eq)

instance Show Expr where 
  show (App e1@(Lamd n e) e2) = "("++show e1++")"++show e2
  show (App e1 e2@(Lamd n e)) = (show e1)++"("++ show e2 ++")"
  show (App e1 e2@(App a b))  = (show e1)++"("++ show e2 ++")"
  show (App e1 e2) = (show e1)++(show e2)
  show (Var n) = "[" ++ show n ++ "]"
  show (Gen n) 
    | n < 26    = [['a'..'z'] !! n]
    | otherwise = "<" ++ show n ++ ">"
  show (Lamd n e)  = (replicate n 'λ') ++ show e

apply :: (Expr -> Expr) -> Expr -> Expr
apply f (App e1 e2) = App (apply f e1) (apply f e2)
apply f (Lamd lv e) = Lamd lv (apply f e)
apply f e           = f e

list2app :: [Expr] -> Expr
list2app [e]      = e
list2app (a:b:es) = list2app $ (App a b):es

allowedChar :: [Char] 
allowedChar = ['I', 'K', 'S', 'B', 'C']

char2bird :: Char -> Expr
char2bird c
  | c == 'I' = I
  | c == 'K' = K
  | c == 'S' = S
  | c == 'B' = B
  | c == 'C' = C
