module Main where

import MyLib
import Control.Monad (unless)
import System.IO

help :: IO ()
help = do 
  putStrLn "Welcome to the bird forest"
  putStrLn ""
  putStrLn "As for now, you can call out the following birds"
  putStrLn "I : Idiot"
  putStrLn "S : Starling"
  putStrLn "K : Kestrel"
  putStrLn "B : Bluebird"
  putStrLn "C : Cardinal"
  putStrLn ""
  putStrLn "Most of the birds could be found in the forest, although they are unnamed"
  putStrLn "Some birds might not terminate"

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
eval' = show . sugaruate 
