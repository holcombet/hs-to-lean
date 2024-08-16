# hs-to-lean

The current goal of this project is to be able to parse haskell source code into an Abstract Syntax Tree. At present, the only output achieved is the contents of the target file (in this case, Test.hs cound in src).

### Before running:
The path of the target file is currently hard-coded in Main.hs. Before running, be sure to change the path of the target file (Test.hs) to match your local filepath.

```
cabal build
cabal run
```