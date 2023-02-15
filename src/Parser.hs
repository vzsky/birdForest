{-# LANGUAGE FlexibleContexts #-}

module Parser 
  ( parseExpr ) where

import Text.Parsec 
import Expr
import Control.Monad (sequence)
import Data.Char

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
knownBird = ['K', 'S', 'I', 'B', 'C']

char2bird :: Char -> Result
char2bird 'K' = Right K
char2bird 'S' = Right S
char2bird 'I' = unnest $ Nest [S, K, K]
char2bird 'B' = unnest $ Nest [S, Nest [K, S], K]
char2bird 'C' = unnest $ Nest [S, Nest [Nest [S, Nest [K, S], K], Nest [S, Nest [K, S], K], S], Nest [K, K]]
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

term :: Parsec String () Term
term =  (B <$> (foldl1 (<|>) $ map char knownBird))
    <|> (G <$> (flip (-) 97) . ord <$> (foldl1 (<|>) $ map char ['a'..'z'] ))
    <|> (G <$> (read <$> (char '<' *> many digit <* char '>')))
    <|> (V <$> (read <$> (char '[' *> many digit <* char ']')))

expr :: Parsec String () ParsedExpr
expr =  (T       <$> term)
    <|> ((L . N) <$> (char 'λ' *> many expr))
    <|> (N       <$> (char '(' *> many expr <* char ')'))

parseExpr :: String -> Result
parseExpr x = case (makeParser expr) ("(" ++ x ++ ")") of 
  Left  a -> Left "parse error"
  Right b -> desugar b
