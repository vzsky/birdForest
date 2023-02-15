module MyLib 
  ( module Expr 
  , module Parser 
  , module Eval
  , evaluate
  , enclyclopedia ) where

import Expr
import Parser
import Eval
import Data.List

deLamd :: [Int] -> Expr -> String
deLamd c e = "λ" ++ getArgs c e ++ "." ++ getBody c e 

getArgs :: [Int] -> Expr -> String
getArgs (c:cs) (Lamd n e)   = show (Gen c) ++ (getArgs cs $ substitute (Gen c) $ joinLamd (n-1) e)
getArgs c e                 = ""

getBody :: [Int] -> Expr -> String
getBody (c:cs) (Lamd n e)        = getBody cs $ substitute (Gen c) $ joinLamd (n-1) e
getBody c (App e1@(Lamd n e) e2) = "("++ deLamd c e1 ++")" ++ getBody c e2
getBody c (App e1 e2@(Lamd n e)) = getBody c e1 ++ "("++ deLamd c e2 ++")"
getBody c (App e1 e2@(App a b))  = getBody c e1 ++ "("++ getBody c e2 ++")"
getBody c (App e1 e2)            = (getBody c e1) ++ (getBody c e2)
getBody c e                      = show e 

avaiGen :: Expr -> [Int]
avaiGen = ([0..] \\) . allGen where 
  allGen (App a b)  = allGen a ++ allGen b
  allGen (Lamd a e) = allGen e
  allGen (Gen x)    = [x]
  allGen e          = []

prettify :: Expr -> String
prettify e@(Lamd n b) = deLamd  (avaiGen e) e 
prettify e            = getBody (avaiGen e) e 

enclyclopedia :: Expr -> String
enclyclopedia e
  | search e == show e = prettify e
  | otherwise          = search e ++ " : " ++ prettify e

search :: Expr -> String
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
