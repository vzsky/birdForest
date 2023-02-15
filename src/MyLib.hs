module MyLib 
  ( module Expr 
  , module Parser 
  , module Eval
  , evaluate
  , enclyclopedia ) where

import Expr
import Parser
import Eval

deLamd :: Int -> Expr -> String
deLamd c e = "λ" ++ getArgs c e ++ "." ++ getBody c e 

getArgs :: Int -> Expr -> String
getArgs c (Lamd n e)  = show (Gen c) ++ (getArgs (c+1) $ substitute (Gen c) $ joinLamd (n-1) e)
getArgs c e           = ""

getBody :: Int -> Expr -> String
getBody c (Lamd n e)             = getBody (c+1) $ substitute (Gen c) $ joinLamd (n-1) e
getBody c (App e1@(Lamd n e) e2) = "("++ deLamd c e1 ++")" ++ getBody c e2
getBody c (App e1 e2@(Lamd n e)) = getBody c e1 ++ "("++ deLamd c e2 ++")"
getBody c (App e1 e2@(App a b))  = getBody c e1 ++ "("++ getBody c e2 ++")"
getBody c (App e1 e2)            = (getBody c e1) ++ (getBody c e2)
getBody c e                      = show e 

maxGen :: Expr -> Int
maxGen (App a b)  = max (maxGen a) (maxGen b)
maxGen (Lamd a e) = maxGen e
maxGen (Gen x)    = x
maxGen e          = -1

prettify :: Expr -> String
prettify e@(Lamd n b) = deLamd  ((maxGen e)+1) e 
prettify e            = getBody ((maxGen e)+1) e 

enclyclopedia :: Expr -> String
enclyclopedia e | search e == show e   = prettify e
                | otherwise            = search e ++ " : " ++ prettify e

search (App e1 e2) = "("++search e1++")" ++ "("++search e2++")"
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
evaluate s = parseExpr s >>= interp
