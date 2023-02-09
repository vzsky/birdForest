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

Most of the birds could be found in the forest, although they are unnamed. Some birds might not terminate

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
- desugar turns expression into the one with just `S`, `K`, and `I`
  - in fact `S` and `K` are good enough
- intepretation break down each bird into the equivalent lambda expression with de bruijn index
- interpretation takes care of reductions and result in a non-reducible lambda expression
- (re)sugar map back the expression to its name 
  - ie. map `λx.x` or `λ0` to `I`
  - ie. map `λxy.x` or `λλ0` to `K`