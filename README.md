# hs-to-lean

**CURRENTLY UNDER CONSTRUCTION**

***

## Description

The current goal of this project is to be able to parse haskell source code into an Abstract Syntax Tree. At present, the only output achieved is the contents of the target file (in this case, Test.hs found in src).

At present, this project is translating the HeapSort algorithm (src/HeapSort.hs) into Lean. 

## Dependencies

Tested with ghc 9.4.8

## Compilation

In the parent directory (hs-to-lean), run the program 
```
cabal build
cabal run > "output.txt"
```

See src/result.txt for Lean translation

