# LeanOutputs

The Lean translation of the target Haskell file(s) are written to this directory.

The .lean files are runnable and testable, but may result in unexpected errors because they are not written to in a "Lean project". 

To create a Lean project, see Lean's ["Lean Projects" page](https://leanprover-community.github.io/install/project.html).


## Running a Lean program:

If the Lean program contains a `main` function, you can execute `main` with the following command-line command:
```
lean --run <file>
```