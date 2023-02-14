module Main (main) where

import Test.Hspec
import MyLib

main :: IO ()
main = hspec $ do

  describe "parser" $ do

    it "parses Expr" $ do
      parseExpr "(S)"         `shouldBe` (unnest . Nest) [S]
      parseExpr "(((KS)))"    `shouldBe` (unnest . Nest) [K, S]
      parseExpr "(K(SKS)K)"   `shouldBe` (unnest . Nest) [K, Nest [S, K, S], K]
      parseExpr "(K(SK)(KS))" `shouldBe` (unnest . Nest) [K, Nest [S, K], Nest [K, S]]
      parseExpr "SK(SK)S"     `shouldBe` (unnest . Nest) [S, K, Nest [S, K], S]

  describe "interp" $ do
      
    it "reduces SK correctly" $ do
      interp (App (App K (App (App S K) K)) S)            `shouldBe` Right (Lamd 1 (Var 0))
      interp (App (App K S) (App (App S K) K))            `shouldBe` Right (Lamd 3 $ App (App (Var 2) (Var 0)) (App (Var 1) (Var 0)))
      interp (App (App K (Gen 0)) (Gen 1))                `shouldBe` Right (Gen 0)
      interp (App (App S K) K)                            `shouldBe` Right (Lamd 1 (Var 0))
      interp (App (App S K) S)                            `shouldBe` Right (Lamd 1 (Var 0))
      interp (App (App (App S K) S) K)                    `shouldBe` Right (Lamd 2 (Var 1))
      interp (App (App (App S K) K) K)                    `shouldBe` Right (Lamd 2 (Var 1))
      interp (App (App (App S K) S) S)                    `shouldBe` Right (Lamd 3 $ App (App (Var 2) (Var 0)) (App (Var 1) (Var 0)))
      interp (App (App (App S K) S) (Gen 2))              `shouldBe` Right (Gen 2)
      interp (App (App (App S K) (Gen 1)) (Gen 2))        `shouldBe` Right (Gen 2)
      interp (App (App (App S (Gen 0)) (Gen 1)) (Gen 2))  `shouldBe` Right (App (App (Gen 0) (Gen 2)) (App (Gen 1) (Gen 2)))

  -- describe "desugar" $ do
  --   it "should be correct" $ do
  --     desugarize <$> tryParse "I" `shouldBe` Right (App (App S K) K)
  --     desugarize <$> tryParse "B" `shouldBe` Right (App (App S (App K S)) K)

  -- describe "evaluate" $ do
  --   it "evaluates SKI correctly" $ do
  --     evaluate "SKSK"   `shouldBe` Right (Lamd 2 (Var 1))
  --     evaluate "SSKI"   `shouldBe` Right (Lamd 1 (App (Var 0) (Lamd 1 (Var 0))))
  --     evaluate "SISKII" `shouldBe` Right (Lamd 1 (Var 0))
  --     evaluate "SKK"    `shouldBe` Right (Lamd 1 (Var 0))
  --     evaluate "S(KS)K" `shouldBe` Right (Lamd 3 (App (Var 2) (App (Var 1) (Var 0))))
  --     evaluate "S(SK)"  `shouldBe` Right (Lamd 2 (App (Var 1) (Var 0)))
  --     evaluate "SII"    `shouldBe` Right (Lamd 1 (App (Var 0) (Var 0)))
  --     evaluate "SI"     `shouldBe` Right (Lamd 2 (App (Var 0) (App (Var 1) (Var 0))))
  --     evaluate "KI"     `shouldBe` Right (Lamd 2 (Var 0))
  --     evaluate "SK"     `shouldBe` Right (Lamd 2 (Var 0))

  --   it "evaluate B correctly" $ do
  --     evaluate "BBB"        `shouldBe` Right (Lamd 4 (App (Var 3) (list2app [(Var 2), (Var 1), (Var 0)])))
  --     evaluate "S(BBS)(KK)" `shouldBe` Right (Lamd 3 $ list2app [(Var 2), (Var 0), (Var 1)])
    
  --   it "evaluate C correctly" $ do
  --     evaluate "C"          `shouldBe` Right (Lamd 3 $ list2app [(Var 2), (Var 0), (Var 1)])
  --     evaluate "CB(SII)"    `shouldBe` Right (Lamd 2 $ App (Var 1) (App (Var 0) (Var 0)))
  --     evaluate "CI"         `shouldBe` Right (Lamd 2 $ App (Var 0) (Var 1))
