module MyLib 
  ( module Expr 
  , module Parser 
  , module Eval
  , evaluate
  , enclyclopedia ) where

import Expr
import Parser
import Eval


prettify :: Expr -> String
prettify = del 0 where
  del  c e                      = "λ" ++ args c e ++ "." ++ body c e 
  args c (Lamd n e)             = show (Gen c) ++ (args (c+1) $ substitute (Gen c) $ joinLamd (n-1) e)
  args c e                      = ""
  body c (Lamd n e)             = body (c+1) $ substitute (Gen c) $ joinLamd (n-1) e
  body c (App e1@(Lamd n e) e2) = "("++ del c e1 ++")" ++ body c e2
  body c (App e1 e2@(Lamd n e)) = body c e1 ++ "("++ del c e2 ++")"
  body c (App e1 e2@(App a b))  = body c e1 ++ "("++ body c e2 ++")"
  body c (App e1 e2)            = (body c e1) ++ (body c e2)
  body c e                      = show e 

enclyclopedia :: Expr -> String
enclyclopedia e = 
  if search e == show e then prettify e
                        else search e ++ " : " ++ prettify e
search (App e1 e2) = "("++(search e1)++")" ++ "("++(search e2)++")"
search e 
    | (Right e) == (evaluate "I")      = "I"
    | (Right e) == (evaluate "S")      = "S"
    | (Right e) == (evaluate "K")      = "K"
    | (Right e) == (evaluate "B")      = "B"
    | (Right e) == (evaluate "C")      = "C"
    | (Right e) == (evaluate "KI")     = "KI"
    | (Right e) == (evaluate "BBB")    = "BBB"
    | (Right e) == (evaluate "BB")     = "D"
    | (Right e) == (evaluate "CI")     = "T"
    | (Right e) == (evaluate "BC(CI)") = "V"
    | (Right e) == (evaluate "SII")    = "M"
    | (Right e) == (evaluate "SI")     = "O"
    | (Right e) == (evaluate "BI")     = "I*"
    | otherwise                        = show e

evaluate :: String -> Result
evaluate s = do 
  p <- parseExpr s 
  interp p
