# hs-to-lean

The current goal of this project is to be able to parse haskell source code into an Abstract Syntax Tree. At present, the only output achieved is the contents of the target file (in this case, Test.hs found in src).

### Dependencies

Tested with ghc 9.4.8

### Compilation

In the parent directory (hs-to-lean), run the program 
```
cabal build
cabal run 
```

**NOTE:** At present, the program is designed to output the translated Lean code into a file named "result.txt" in src.


## Relevant Developmental Notes:

Current limitations:
* translation of Haskell functions into Lean are implemented for only pattern matching functions (in FunBind, src/TranslateHaskell.hs) and does not account for functions that do not require direct pattern matching
