module MyLib 
  ( module Expr 
  , module Parser 
  , module Eval
  , evaluate
  , enclyclopedia ) where

import Expr
import Parser
import Eval

enclyclopedia :: Expr -> String
enclyclopedia e = 
  if search e == show e then show e
                        else search e ++ " which is " ++ show e
search (App e1 e2) = "("++(search e1)++")" ++ "("++(search e2)++")"
search e 
    | (Just e) == (evaluate "I")      = "I"
    | (Just e) == (evaluate "S")      = "S"
    | (Just e) == (evaluate "K")      = "K"
    | (Just e) == (evaluate "B")      = "B"
    | (Just e) == (evaluate "C")      = "C"
    | (Just e) == (evaluate "KI")     = "KI"
    | (Just e) == (evaluate "BBB")    = "BBB"
    | (Just e) == (evaluate "BB")     = "D"
    | (Just e) == (evaluate "CI")     = "T"
    | (Just e) == (evaluate "BC(CI)") = "V"
    | (Just e) == (evaluate "SII")    = "M"
    | (Just e) == (evaluate "SI")     = "O"
    | (Just e) == (evaluate "BI")     = "I*"
    | otherwise                       = show e

evaluate = (eval <$>) . tryParse