module Expr 
  ( Expr(..)
  , char2bird
  , allowedChar
  , list2app
  , apply ) where


data Expr 
  = App Expr Expr
  | B | C
  | I | K | S
  | Var Int
  | Cont Int Expr
  | Gen Int
  deriving (Eq)

instance Show Expr where 
  show (App e1 e2) = "("++(show e1)++(show e2)++")"
  show (Var n) = "[" ++ show n ++ "]"
  show (Gen n) = "<" ++ show n ++ ">"
  show (Cont n e)  = "{λ" ++ show n ++ show e ++ "}"
  show I = "I"
  show K = "K"
  show S = "S"
  show B = "B"
  show C = "C"


apply :: (Expr -> Expr) -> Expr -> Expr
apply f (App e1 e2) = App (apply f e1) (apply f e2)
apply f (Cont lv e) = Cont lv (apply f e)
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
