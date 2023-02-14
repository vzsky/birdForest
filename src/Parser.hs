{-# LANGUAGE FlexibleContexts #-}

module Parser 
  ( parseExpr
  ) where

import Text.Parsec 
import Expr
import Control.Monad (sequence)

data Term 
  = B Char
  | G Int 
  | V Int

data ParsedExpr 
  = T Term 
  | L ParsedExpr
  | N [ParsedExpr]

makeParser p = parse p ""

knownBird :: [Char] 
knownBird = ['K', 'S']

char2bird :: Char -> Result
char2bird 'K' = Right K
char2bird 'S' = Right S
char2bird  c  = Left "unknown bird marked as known"

desugar :: ParsedExpr -> Result
desugar (T (B c))  = char2bird c
desugar (T (G c))  = Right $ Gen c
desugar (T (V c))  = Right $ Var c
desugar (L e)      = do 
  b <- desugar e
  return $ joinLamd 1 b
desugar (N es) = do 
  s <- sequence $ map desugar es
  unnest $ Nest s 

bird :: Parsec String () Term 
bird = B <$> (foldl1 (<|>) $ map char knownBird)

genr :: Parsec String () Term
genr = G <$> (read <$> (char '<' *> many digit <* char '>'))

varb :: Parsec String () Term
varb = V <$> (read <$> (char '[' *> many digit <* char ']'))

term :: Parsec String () Term
term = bird <|> genr <|> varb

expr :: Parsec String () ParsedExpr
expr =  (T <$> term)
    <|> do 
      char 'λ'
      e <- many expr
      return $ L $ N e 
    <|> do 
      char '('
      e <- many expr
      char ')'
      return $ N e 

parseExpr :: String -> Result
parseExpr x = case (makeParser expr) ("(" ++ x ++ ")") of 
  Left  a -> Left "parse error"
  Right b -> desugar b
