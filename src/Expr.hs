module Expr 
  ( Expr(..)
  , Result
  , char2bird
  , allowedChar
  , list2app
  , unnest ) where

type Result = Either String Expr

data Expr 
  = App Expr Expr
  | K | S 
  | Var Int
  | Lamd Int Expr
  | Gen Int
  | Nest [Expr]
  deriving (Eq)

-- apply :: (Expr -> Expr) -> Expr -> Expr
-- apply f (App e1 e2) = App (apply f e1) (apply f e2)
-- apply f (Lamd lv e) = Lamd lv (apply f e)
-- apply f e           = f e

-- desugarize :: Expr -> Expr
-- desugarize = apply f where 
--   f I = list2app [S, K, K]
--   f B = list2app [S, App K S, K]
--   f C = desugarize $ list2app [S, list2app [B, B, S], list2app [K, K]]
--   f e = e

instance Show Expr where 
  show (App e1@(Lamd n e) e2) = "("++show e1++")"++show e2
  show (App e1 e2@(Lamd n e)) = (show e1)++"("++ show e2 ++")"
  show (App e1 e2@(App a b))  = (show e1)++"("++ show e2 ++")"
  show (App e1 e2)            = (show e1)++(show e2)
  show (Var n)                = "[" ++ show n ++ "]"
  show (Gen n)  | n < 26      = [['a'..'z'] !! n]
                | otherwise   = "<" ++ show n ++ ">"
  show (Lamd n e)             = (replicate n 'λ') ++ show e
  show (Nest es)              = show es 
  show S                      = "S"
  show K                      = "K"

-- @deprecated
list2app :: [Expr] -> Expr
list2app [e]      = e
list2app (a:b:es) = list2app $ (App a b):es

unnest :: Expr -> Result
unnest (Nest [])      = Left "cannot unnest empty nest"
unnest (Nest [e])     = Right e
unnest (Nest (a:b:e)) = unnest $ Nest $ (App a b):e
unnest e              = Right e

-- @deprecated
allowedChar :: [Char] 
allowedChar = ['K', 'S']

-- @deprecated
char2bird :: Char -> Expr
char2bird c
  | c == 'K' = K
  | c == 'S' = S