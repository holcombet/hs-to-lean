# hs-to-lean

The current goal of this project is to be able to parse haskell source code into an Abstract Syntax Tree. At present, the only output achieved is the contents of the target file (in this case, Test.hs found in src).

### Dependencies

Tested with ghc 9.4.8

### Compilation

In the parent directory (hs-to-lean), run the program 
```
cabal build
cabal run > "output.txt"
```

**NOTE:** Multiple output files are generated when running this program to promote visualization during the testing and development process. Current outputs include:
* output.txt : the result of parsing the AST back into Haskell (from `cabal run > "output.txt"`)
* AST.txt : the raw AST generated from parsing using ghc-lib-parser 
* hs_structured_AST.txt : the structured AST, derived from AST.txt 
* src/result.txt : the result of parsing the AST into Lean

## Relevant Developmental Notes:

Current limitations:
* translation of Haskell functions into Lean are implemented for only pattern matching functions (in FunBind, src/TranslateHaskell.hs) and does not account for functions that do not require direct pattern matching
