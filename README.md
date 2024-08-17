# hs-to-lean

The current goal of this project is to be able to parse haskell source code into an Abstract Syntax Tree. At present, the only output achieved is the contents of the target file (in this case, Test.hs cound in src).

### Dependencies

Tested with ghc 9.4.8

### Compilation

In the parent directory (hs-to-lean), run the program 
```
cabal build
cabal run
```