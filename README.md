# Bird Forest

An implementation of bird forest inspired from _To mock a mocking bird_. 

A lambda calculus and combinatory logic interpreter.

----

As for now, you can call out the following birds
- I : Idiot
- S : Starling
- K : Kestrel
- B : Bluebird
- C : Cardinal

Most of the birds could be found in the forest, although they are unnamed. Some birds might not terminate, for example, the Y-combinator bird `(S(CB(SII))(CB(SII)))`.

The REPL can parse mentioned birds and lambda expression using the de bruijn index notation.

### Examples 
  - `SKK` will be parsed as `(SK)K` and return `I`
  - `λ[0]` means `λx.x`
  - `λλλ[0][1]` means `λxyz.zy`

----

## Using
1. download haskell and cabal
2. `cabal run birdForest` to open a repl

----
## Test
- run `cabal test` to run the test

----
## Implementation Detail
All library files are in `/src/`, tests are in `/test/` and repl is in `/app/`
### Parsing 
- Implemented parsing certain birds listed above
- Parsing detail specified in `Expr.hs`
- Parsing algorithm in `Parser.hs`
### Evaluation
- splited into intepret, (re)sugar
- intepretation break down each bird into the equivalent lambda expression with de bruijn index
- interpretation takes care of reductions and result in a non-reducible lambda expression
- encyclopedia map back the expression to its name 
  - ie. map `λx.x` or `λ0` to `I`
  - ie. map `λxy.x` or `λλ1` to `K`
- prettify by changing de bruijn index notation into named lambda

## Library Specification

`Expr` is a type that represent the syntax of the language. 

`Result` is a type of `Either String Expr` where the string is the error message 
### Parser 
1. `parseExpr :: String -> Result` 
   - parse string into a `Result`
### Evaluation
1. `interp :: Expr -> Result`
   -  makes reduction of any Expression containing `S` and `K`
2. `evaluate :: String -> Result`
   - _parse_ the string, and do `interp`.
### Output 
1. `encyclopedia :: Expr -> String` 
   - yields the prettiest response with bird name and named lambda expression
2. `prettify :: Expr -> String` 
   - returns the corresponding named lambda expression 
3. `show :: Expr -> String` 
   - returns the lambda expression in de bruijn index style 
## Reference
[birds list](https://www.angelfire.com/tx4/cus/combinator/birds.html)