{-# LANGUAGE FlexibleContexts #-}

module Parser 
  ( parseBird
  , parseExpr
  , tryParse
  ) where

import Text.Parsec 
import Expr

makeParser p = parse p ""

bird :: Parsec String () Expr
bird = char2bird <$> (foldl1 (<|>) $ map char allowedChar)
parseBird = makeParser bird

expr :: Parsec String () Expr
expr = bird <|> list2app <$> (char '(' *> many expr <* char ')')
parseExpr = makeParser expr



tryParse :: String -> Maybe Expr
tryParse x = case parseExpr ("(" ++ x ++ ")") of
  Left  a -> Nothing
  Right b -> Just b