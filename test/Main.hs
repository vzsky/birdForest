module Main (main) where

import Test.Hspec
import MyLib

main :: IO ()
main = hspec $ do

  describe "parser" $ do
    it "parses Bird" $ do
      parseBird "I" `shouldBe` Right (I)
      parseBird "K" `shouldBe` Right (K)
      parseBird "S" `shouldBe` Right (S)

    it "parses Maybe" $ do
      tryParse "(S)"         `shouldBe` Just (S)
      tryParse "(((KI)))"    `shouldBe` Just (App K I)
      tryParse "(K(III)K)"   `shouldBe` Just (App (App K (App (App I I) I)) K)
      tryParse "(K(IK)(KI))" `shouldBe` Just (App (App K (App I K)) (App K I))
      tryParse "SK(SK)I"     `shouldBe` Just (App (App (App S K) (App S K)) I)

  describe "interp" $ do
    it "reduces I correctly" $ do
      interp I                          `shouldBe` (Lamd 1 (Var 0))
      interp (App I I)                  `shouldBe` (Lamd 1 (Var 0))
      interp (App I (App I (App I I)))  `shouldBe` (Lamd 1 (Var 0))
      interp (App I (Gen 0))            `shouldBe` (Gen 0)
      
    it "reduces K correctly" $ do
      interp (App (App K I) I)             `shouldBe` (Lamd 1 (Var 0))
      interp (App (App K S) I)             `shouldBe` (Lamd 3 $ App (App (Var 0) (Var 2)) (App (Var 1) (Var 2)))
      interp (App (App K (Gen 0)) (Gen 1)) `shouldBe` (Gen 0)

    it "reduces S correctly" $ do
      interp (App (App (App S I) I) I)                    `shouldBe` (Lamd 1 (Var 0))
      interp (App (App (App S K) K) I)                    `shouldBe` (Lamd 1 (Var 0))
      interp (App (App (App S K) K) K)                    `shouldBe` (Lamd 2 (Var 0))
      interp (App (App (App S K) S) S)                    `shouldBe` (Lamd 3 $ App (App (Var 0) (Var 2)) (App (Var 1) (Var 2)))
      interp (App (App (App S K) S) (Gen 2))              `shouldBe` (Gen 2)
      interp (App (App (App S K) (Gen 1)) (Gen 2))        `shouldBe` (Gen 2)
      interp (App (App (App S (Gen 0)) (Gen 1)) (Gen 2))  `shouldBe` (App (App (Gen 0) (Gen 2)) (App (Gen 1) (Gen 2)))

  describe "desugar" $ do
    it "desugarizes B correctly" $ do
      desugarize <$> tryParse "B" `shouldBe` Just (App (App S (App K S)) K)

  describe "evaluate" $ do
    it "evaluates SKI correctly" $ do
      evaluate "SKSK"   `shouldBe` Just (Lamd 2 (Var 0))
      evaluate "SSKI"   `shouldBe` Just (Lamd 1 (App (Var 0) (Lamd 1 (Var 1))))
      evaluate "SISKII" `shouldBe` Just (Lamd 1 (Var 0))
      evaluate "SKK"    `shouldBe` Just (Lamd 1 (Var 0))
      evaluate "S(KS)K" `shouldBe` Just (Lamd 3 (App (Var 0) (App (Var 1) (Var 2))))
      evaluate "S(SK)"  `shouldBe` Just (Lamd 2 (App (Var 0) (Var 1)))
      evaluate "SII"    `shouldBe` Just (Lamd 1 (App (Var 0) (Var 0)))
      evaluate "SI"     `shouldBe` Just (Lamd 2 (App (Var 1) (App (Var 0) (Var 1))))
      evaluate "KI"     `shouldBe` Just (Lamd 2 (Var 1))
      evaluate "SK"     `shouldBe` Just (Lamd 2 (Var 1))

    it "evaluate B correctly" $ do
      evaluate "BBB"        `shouldBe` Just (Lamd 4 (App (Var 0) (list2app [(Var 1), (Var 2), (Var 3)])))
      evaluate "S(BBS)(KK)" `shouldBe` Just (Lamd 3 $ list2app [(Var 0), (Var 2), (Var 1)])
    
    it "evaluate C correctly" $ do
      evaluate "C"          `shouldBe` Just (Lamd 3 $ list2app [(Var 0), (Var 2), (Var 1)])
      evaluate "CB(SII)"    `shouldBe` Just (Lamd 2 $ App (Var 0) (App (Var 1) (Var 1)))
      evaluate "CI"         `shouldBe` Just (Lamd 2 $ App (Var 1) (Var 0))

  describe "sugar" $ do
    it "sugarizes bird correctly" $ do 
      sugaruate "C" `shouldBe` Just (C)
      sugaruate "S" `shouldBe` Just (S)
      sugaruate "I" `shouldBe` Just (I)
      sugaruate "K" `shouldBe` Just (K)
      sugaruate "B" `shouldBe` Just (B)

    it "sugarizes expression correctly" $ do 
      sugaruate "SKK" `shouldBe` Just (I)
      sugaruate "SKS" `shouldBe` Just (I)
      sugaruate "KKS" `shouldBe` Just (K)
      sugaruate "KSB" `shouldBe` Just (S)