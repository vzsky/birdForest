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
enclyclopedia e
  | (Just e) == (evaluate "I")      = "I: Idiot"
  | (Just e) == (evaluate "S")      = "S: Starling"
  | (Just e) == (evaluate "K")      = "K: Kestral"
  | (Just e) == (evaluate "B")      = "B: Bluebird"
  | (Just e) == (evaluate "C")      = "C: Cardinal"
  | (Just e) == (evaluate "KI")     = "KI: Kite"
  | (Just e) == (evaluate "BBB")    = "BBB: Blackbird"
  | (Just e) == (evaluate "BB")     = "D: Dove"
  | (Just e) == (evaluate "CI")     = "T: Thrush"
  | (Just e) == (evaluate "BC(CI)") = "V: Vireo"
  | (Just e) == (evaluate "SII")    = "M: Mockingbird"
  | (Just e) == (evaluate "SI")     = "O: Owl"
  | (Just e) == (evaluate "BI")     = "I*: Idiot once removed"
  | otherwise                       = show e

evaluate = (eval <$>) . tryParse