module Main where

import MyLib
import Control.Monad (unless)
import System.IO

main :: IO ()
main = do
  input <- read'
  unless (input == ":quit")
      $ putStrLn (eval' input)
      >> main

read' :: IO String
read' = putStr "> "
     >> hFlush stdout
     >> getLine

eval' :: String -> String
eval' s = case evaluate s of 
  Nothing -> "parse error" 
  Just s  -> enclyclopedia s
