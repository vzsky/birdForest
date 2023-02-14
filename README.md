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

The REPL can only parse Birds but not Lambda Expression (at the moment)

Ex. `SKK` will be parsed as `(SK)K` and return `I`

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
- splited into desugar, intepret, (re)sugar
- desugar turns expression into the one with just `S` and `K`
  - in fact `S`, `K`, `I`, `B`, and `C` would speed up.
- intepretation break down each bird into the equivalent lambda expression with de bruijn index
- interpretation takes care of reductions and result in a non-reducible lambda expression
- encyclopedia map back the expression to its name 
  - ie. map `λx.x` or `λ0` to `I`
  - ie. map `λxy.x` or `λλ1` to `K`
- prettify by changing de bruijn index notation into named lambda

## Library Specification

### Evaluation
1. `interp` makes reduction of any Expression containing `S` and `K`
2. `desugarize` reduces known birds into `S` and `K`
3. `evaluate` _parse_ the string, then `desugarize`, and do `interp`.
### Output 
1. `encyclopedia` yields the prettiest response with bird name and named lambda expression
2. `prettify` returns the corresponding named lambda expression 
3. `show` returns the lambda expression in de bruijn index style 
## Reference
[birds list](https://www.angelfire.com/tx4/cus/combinator/birds.html)