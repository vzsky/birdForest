module MyLib 
  ( module Expr 
  , module Parser 
  , module Eval
  , evaluate
  , sugaruate ) where

import Expr
import Parser
import Eval

evaluate = (eval <$>) . tryParse
sugaruate = (sugarize <$>) . evaluate