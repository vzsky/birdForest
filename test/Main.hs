module Main (main) where

import Test.Hspec
import MyLib
import Data.Function (on)
import Data.Bifunctor

resultTo :: (Show b, Eq b) => Either a b -> Either a b -> Expectation
resultTo = shouldBe `on` (first $ const ())

main :: IO ()
main = hspec $ do

  describe "expr" $ do
    it "could unnest" $ do 
      unnest (Nest [])                  `resultTo` Left ""
      unnest (Nest [Nest [Nest []]])    `resultTo` Left ""
      unnest (Nest [Nest [Nest [S]]])   `resultTo` Right (S)
      unnest (Lamd 1 $ Nest [])         `resultTo` Left ""
      unnest (Lamd 1 $ Nest [S, K])     `resultTo` Right (Lamd 1 (App S K))
      unnest (Nest [S, K, S])           `resultTo` Right (App (App S K) S)
      unnest (Nest [S, Gen 0, Var 1])   `resultTo` Right (App (App S (Gen 0)) (Var 1))
      unnest (Nest [S, S, Nest [S, S]]) `resultTo` Right (App (App S S) (App S S))

  describe "parser" $ do
    it "parses Bird Sequence" $ do
      parseExpr "(S)"         `resultTo` (unnest . Nest) [S]
      parseExpr "(((KS)))"    `resultTo` (unnest . Nest) [K, S]
      parseExpr "(K(SKS)K)"   `resultTo` (unnest . Nest) [K, Nest [S, K, S], K]
      parseExpr "(K(SK)(KS))" `resultTo` (unnest . Nest) [K, Nest [S, K], Nest [K, S]]
      parseExpr "SK(SK)S"     `resultTo` (unnest . Nest) [S, K, Nest [S, K], S]

    it "parses Term" $ do 
      parseExpr "[0]"   `resultTo` Right (Var 0)
      parseExpr "[123]" `resultTo` Right (Var 123)
      parseExpr "[000]" `resultTo` Right (Var 0)
      parseExpr "<0>"   `resultTo` Right (Gen 0)
      parseExpr "<88>"  `resultTo` Right (Gen 88)
      parseExpr "<000>" `resultTo` Right (Gen 0)
      parseExpr "a"     `resultTo` Right (Gen 0)
      parseExpr "z"     `resultTo` Right (Gen 25)
      parseExpr "t"     `resultTo` Right (Gen 19)

    it "parses Lambda" $ do 
      parseExpr "λ[0]"          `resultTo` unnest (Lamd 1 $ Nest [Var 0])
      parseExpr "λ[0][0]"       `resultTo` unnest (Lamd 1 $ Nest [Var 0, Var 0])
      parseExpr "λλ[0][1]"      `resultTo` unnest (Lamd 2 $ Nest [Var 0, Var 1])
      parseExpr "λλ[3][3]"      `resultTo` unnest (Lamd 2 $ Nest [Var 3, Var 3])
      parseExpr "λλ[0]λ[0]"     `resultTo` unnest (Lamd 2 $ Nest [Var 0, Lamd 1 (Var 0)])
      parseExpr "λλ([0]λ[0])"   `resultTo` unnest (Lamd 2 $ Nest [Var 0, Lamd 1 (Var 0)])
      parseExpr "λ(λ[0])(λ[0])" `resultTo` unnest (Lamd 1 $ Nest [Lamd 1 (Var 0), Lamd 1 (Var 0)])
      parseExpr "λS"            `resultTo` unnest (Lamd 1 $ Nest [S])
      parseExpr "λλK"           `resultTo` unnest (Lamd 2 $ Nest [K])
      parseExpr "λλSKK"         `resultTo` unnest (Lamd 2 $ Nest [S, K, K])
      parseExpr "λλS[0]<0>"     `resultTo` unnest (Lamd 2 $ Nest [S, Var 0, Gen 0])
      parseExpr "λλabc"         `resultTo` unnest (Lamd 2 $ Nest [Gen 0, Gen 1, Gen 2])
      parseExpr "λλxyz"         `resultTo` unnest (Lamd 2 $ Nest [Gen 23, Gen 24, Gen 25])

    it "desugar I" $ do 
      parseExpr "I"       `resultTo` parseExpr "SKK"
      parseExpr "λI"      `resultTo` parseExpr "λSKK"
      parseExpr "λI[0]"   `resultTo` parseExpr "λ(SKK)[0]"
      parseExpr "λ<0>I"   `resultTo` parseExpr "λ<0>(SKK)"

    it "desugar B" $ do
      parseExpr "B"       `resultTo` parseExpr "S(KS)K"
      parseExpr "λB"      `resultTo` parseExpr "λS(KS)K"
      parseExpr "λB[0]"   `resultTo` parseExpr "λS(KS)K[0]"
      parseExpr "λ<0>B"   `resultTo` parseExpr "λ<0>(S(KS)K)"

    it "desugar C" $ do
      parseExpr "C"       `resultTo` parseExpr "S(BBS)(KK)"
      parseExpr "λC"      `resultTo` parseExpr "λ(S(BBS)(KK))"
      parseExpr "C<0>"    `resultTo` parseExpr "(S(BBS)(KK))<0>"

  describe "interp" $ do
    it "reduces SK correctly" $ do
      interp (App (App K (App (App S K) K)) S)            `resultTo` Right (Lamd 1 (Var 0))
      interp (App (App K S) (App (App S K) K))            `resultTo` Right (Lamd 3 $ App (App (Var 2) (Var 0)) (App (Var 1) (Var 0)))
      interp (App (App K (Gen 0)) (Gen 1))                `resultTo` Right (Gen 0)
      interp (App (App S K) K)                            `resultTo` Right (Lamd 1 (Var 0))
      interp (App (App S K) S)                            `resultTo` Right (Lamd 1 (Var 0))
      interp (App (App (App S K) S) K)                    `resultTo` Right (Lamd 2 (Var 1))
      interp (App (App (App S K) K) K)                    `resultTo` Right (Lamd 2 (Var 1))
      interp (App (App (App S K) S) S)                    `resultTo` Right (Lamd 3 $ App (App (Var 2) (Var 0)) (App (Var 1) (Var 0)))
      interp (App (App (App S K) S) (Gen 2))              `resultTo` Right (Gen 2)
      interp (App (App (App S K) (Gen 1)) (Gen 2))        `resultTo` Right (Gen 2)
      interp (App (App (App S (Gen 0)) (Gen 1)) (Gen 2))  `resultTo` Right (App (App (Gen 0) (Gen 2)) (App (Gen 1) (Gen 2)))

  describe "evaluate" $ do
    it "evaluates SKI correctly" $ do
      evaluate "SKSK"   `resultTo` Right (Lamd 2 (Var 1))
      evaluate "SSKI"   `resultTo` Right (Lamd 1 (App (Var 0) (Lamd 1 (Var 0))))
      evaluate "SISKII" `resultTo` Right (Lamd 1 (Var 0))
      evaluate "SKK"    `resultTo` Right (Lamd 1 (Var 0))
      evaluate "S(KS)K" `resultTo` Right (Lamd 3 (App (Var 2) (App (Var 1) (Var 0))))
      evaluate "S(SK)"  `resultTo` Right (Lamd 2 (App (Var 1) (Var 0)))
      evaluate "SII"    `resultTo` Right (Lamd 1 (App (Var 0) (Var 0)))
      evaluate "SI"     `resultTo` Right (Lamd 2 (App (Var 0) (App (Var 1) (Var 0))))
      evaluate "KI"     `resultTo` Right (Lamd 2 (Var 0))
      evaluate "SK"     `resultTo` Right (Lamd 2 (Var 0))

    it "evaluate B correctly" $ do
      evaluate "BBB"        `resultTo` unnest (Lamd 4 $ Nest [Var 3, Nest [Var 2, Var 1, Var 0]])
      evaluate "S(BBS)(KK)" `resultTo` unnest (Lamd 3 $ Nest [Var 2, Var 0, Var 1])
    
    it "evaluate C correctly" $ do
      evaluate "C"          `resultTo` unnest (Lamd 3 $ Nest [Var 2, Var 0, Var 1])
      evaluate "CB(SII)"    `resultTo` unnest (Lamd 2 $ Nest [Var 1, Nest [Var 0, Var 0]])
      evaluate "CI"         `resultTo` unnest (Lamd 2 $ Nest [Var 0, Var 1])
