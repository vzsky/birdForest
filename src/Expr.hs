module Expr 
  ( Expr(..)
  , Result
  , unnest
  , joinLamd ) where

type Result = Either String Expr

data Expr 
  = App Expr Expr
  | K | S 
  | Var Int
  | Lamd Int Expr
  | Gen Int
  | Nest [Expr]
  deriving (Eq)

instance Show Expr where 
  show (App e1@(Lamd n e) e2) = "("++show e1++")"++show e2
  show (App e1 e2)            = (show e1)++(circ e2)
    where 
      circ e@(Lamd n x)       = "("++show e++")"
      circ e@(App a b)        = "("++show e++")"
      circ e                  = show e

  show (Var n)                = "[" ++ show n ++ "]"
  show (Gen n)  | n < 26      = [['a'..'z'] !! n]
                | otherwise   = "<" ++ show n ++ ">"
  show (Lamd n e)             = (replicate n 'λ') ++ show e
  show (Nest es)              = "{" ++ (foldl1 (++) $ map show es) ++ "}" 
  show S                      = "S"
  show K                      = "K"

unnest :: Expr -> Result
unnest (Nest [])      = Left "cannot unnest empty nest"
unnest (Nest [e])     = unnest e
unnest (Nest (a:b:e)) = do 
  x <- unnest a
  y <- unnest b
  unnest $ Nest $ (App x y):e
unnest e              = Right e

joinLamd :: Int -> Expr -> Expr
joinLamd 0 e          = e
joinLamd n (Lamd l e) = (Lamd (n+l) e)
joinLamd n e          = (Lamd n     e)