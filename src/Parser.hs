{-# LANGUAGE FlexibleContexts #-}

module Parser 
  ( parseBird
  , parseExpr
  , tryParse
  ) where

import Text.Parsec 
import Expr

makeParser p = parse p ""

-- @deprecated
bird :: Parsec String () Expr
bird = char2bird <$> (foldl1 (<|>) $ map char allowedChar)
parseBird = makeParser bird

-- @deprecated
expr :: Parsec String () Expr
expr = bird <|> list2app <$> (char '(' *> many expr <* char ')')
parseExpr = makeParser expr

-- @deprecated
tryParse :: String -> Result
tryParse x = case parseExpr ("(" ++ x ++ ")") of 
  Left  a -> Left "parse error"
  Right b -> Right b
