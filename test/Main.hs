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
      
    it "reduces K correctly" $ do
      interp (App (App K (App (App S K) K)) S)   `shouldBe` (Lamd 1 (Var 0))
      interp (App (App K S) (App (App S K) K))   `shouldBe` (Lamd 3 $ App (App (Var 2) (Var 0)) (App (Var 1) (Var 0)))
      interp (App (App K (Gen 0)) (Gen 1))       `shouldBe` (Gen 0)

    it "reduces S correctly" $ do
      interp (App (App S K) K)                            `shouldBe` (Lamd 1 (Var 0))
      interp (App (App S K) S)                            `shouldBe` (Lamd 1 (Var 0))
      interp (App (App (App S K) S) K)                    `shouldBe` (Lamd 2 (Var 1))
      interp (App (App (App S K) K) K)                    `shouldBe` (Lamd 2 (Var 1))
      interp (App (App (App S K) S) S)                    `shouldBe` (Lamd 3 $ App (App (Var 2) (Var 0)) (App (Var 1) (Var 0)))
      interp (App (App (App S K) S) (Gen 2))              `shouldBe` (Gen 2)
      interp (App (App (App S K) (Gen 1)) (Gen 2))        `shouldBe` (Gen 2)
      interp (App (App (App S (Gen 0)) (Gen 1)) (Gen 2))  `shouldBe` (App (App (Gen 0) (Gen 2)) (App (Gen 1) (Gen 2)))

  describe "desugar" $ do
    it "should be correct" $ do
      desugarize <$> tryParse "I" `shouldBe` Just (App (App S K) K)
      desugarize <$> tryParse "B" `shouldBe` Just (App (App S (App K S)) K)

  describe "evaluate" $ do
    it "evaluates SKI correctly" $ do
      evaluate "SKSK"   `shouldBe` Just (Lamd 2 (Var 1))
      evaluate "SSKI"   `shouldBe` Just (Lamd 1 (App (Var 0) (Lamd 1 (Var 0))))
      evaluate "SISKII" `shouldBe` Just (Lamd 1 (Var 0))
      evaluate "SKK"    `shouldBe` Just (Lamd 1 (Var 0))
      evaluate "S(KS)K" `shouldBe` Just (Lamd 3 (App (Var 2) (App (Var 1) (Var 0))))
      evaluate "S(SK)"  `shouldBe` Just (Lamd 2 (App (Var 1) (Var 0)))
      evaluate "SII"    `shouldBe` Just (Lamd 1 (App (Var 0) (Var 0)))
      evaluate "SI"     `shouldBe` Just (Lamd 2 (App (Var 0) (App (Var 1) (Var 0))))
      evaluate "KI"     `shouldBe` Just (Lamd 2 (Var 0))
      evaluate "SK"     `shouldBe` Just (Lamd 2 (Var 0))

    it "evaluate B correctly" $ do
      evaluate "BBB"        `shouldBe` Just (Lamd 4 (App (Var 3) (list2app [(Var 2), (Var 1), (Var 0)])))
      evaluate "S(BBS)(KK)" `shouldBe` Just (Lamd 3 $ list2app [(Var 2), (Var 0), (Var 1)])
    
    it "evaluate C correctly" $ do
      evaluate "C"          `shouldBe` Just (Lamd 3 $ list2app [(Var 2), (Var 0), (Var 1)])
      evaluate "CB(SII)"    `shouldBe` Just (Lamd 2 $ App (Var 1) (App (Var 0) (Var 0)))
      evaluate "CI"         `shouldBe` Just (Lamd 2 $ App (Var 0) (Var 1))
