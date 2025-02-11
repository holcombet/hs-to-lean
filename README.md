# hs-to-lean

This is a project by Tally Holcombe for her Master's in Electrical Engineering and Computer Science Thesis at Chapman University. This repo is a work-in-progress, and is not expected to be completed until after her program is completed in May of 2025.

The purpose of this project is to convert Haskell programs to their equivalent Lean code.


## Description

The overarching goal of this project is to successfully validate Haskell source code by translating it into the [Lean Theorem Prover](https://lean-lang.org/).

Haskell programs are translated to Lean code by first transforming the Abstract Syntax Tree (AST) generated by GHC's [ghc-lib-parser](https://hackage.haskell.org/package/ghc-lib-parser) API into a simplied algebraic data type (ADT) in [`AST.hs`](https://github.com/holcombet/hs-to-lean/blob/main/src/AST.hs). The structure of the simplified AST is more abstracted compared to its predecessor for a number of reasons. 

First, our ADT is based on ghc-lib-parser's [`ParsedModule`](https://hackage.haskell.org/package/ghc-lib-9.10.1.20250103/docs/GHC.html#t:ParsedModule) AST, which reserves and preserves data for further typechecking and desugaring. This extra data is not required by our compiler because we are able to assume the Haskell source code has already been type checked during its own writing and compilation process. Thus, we are able to abstract the structures further, so our ADT contains only the relevant structures for translation. 

Second, much of `ghc-lib-parser`'s `ParsedModule` structure is redundant in relation to our needs. We took the liberty to combine and modify many of its constructors and data types to simplify the AST into only its essential structures. This significantly reduces the "depth" of the AST, resulting in less complicated traversal across structures and a more straightforward linearization process.

Additionally, our AST is structured in a way that is not necessarily specific to Lean's syntax. Our intent is to provide the ability to linearize our AST to a selection of languages, and thus act as a common abstract syntax between Haskell and various theorem provers, including Lean, Coq, and Agda. 


## Related Work

This project is inspired by the work of Antal Spector-Zabusky, Joachim Breitner, Christine Rizkallah, and Stephanie Weirich in their paper ["Total Haskell is Reasonable Coq"](https://arxiv.org/abs/1711.09286). Their compiler can be found [here](https://github.com/plclub/hs-to-coq).

### Other relevant work:
* Abel et al.'s  ["Verifying haskell programs using constructive type theory"](https://dl.acm.org/doi/10.1145/1088348.1088355) (2005) translated Haskell programs into (a monadic form of) Agda using GHC's Core language. 
* Carr et al.'s ["An approach to translating Haskell programs to
Agda and reasoning about them"](https://arxiv.org/abs/2205.08718) (2022) extended Agda's capabilities to support Haskell features that Agda originally lacks, allowing translations from Haskell to Agda to be more semantically similar.


## Dependencies

This project requires the following packages: 
* ghc 9.4.8
* ghc-lib-parser
* syb
* filepath

## Compilation


### Compiling with the default program
In the parent directory (hs-to-lean), run the following command to compile and translate the **default** Haskell program, `examples/HeapSort.hs`:
```
cabal build
cabal run hs-to-lean
```

### Compiling a specific program
To compile an translate any other Haskell program, run the following command (in the parent directory):

```
cabal build
cabal run hs-to-lean <filepath> 
```

### The `examples` directory
Pre-existing, testable Haskell programs can all be found in the `examples` directory.  All files intended for translation should be in the `examples` directory. All filepaths entered during compilation should be of the format `examples/...`. For more information about the example source files, see [`examples/README.md`](https://github.com/holcombet/hs-to-lean/blob/main/examples/README.md).

### The `LeanOutputs` directory
The Lean translation is written to a lean file and saved in the `LeanOutputs` directory. The Lean file is automatically named, using the module name of the translated Haskell file. 


### Translating your own files

If you wish to translate your own Haskell program, make sure your script contains a **module description** at the top of the file. For example:

```haskell
module FileName where
...
```
## Main Components of hs-to-lean

This compiler currently consists of four main components:

* `app/Main.hs`: parses the target Haskell file and linearizes the intermediary ADT into Lean code  \
    The transformation steps are as follows:\
    (`Haskell` $\to$ `ghc-lib-parser's AST` $\to$ `Intermediary ADT/AST` $\to$ `Lean`)

* `src/TestAST.hs`: The ADT declarations to generate the intermediate AST

* `src/HsToLean/ProcessFile.hs`: Translates `ghc-lib-parser`'s AST structure into the ADT/AST defined in `src/TestAST.hs`

* `src/HsToLean/ASTToLean.hs`: Linearizes the ADT/AST generated by `src/HsToLean/ProcessFile.hs` into Lean code. 


## Notable Limitations

At present, this tool can translate only a fragment of Haskell. The following list contains some of the notable limitations to this compiler:
* **Using typeclasses**: Only the Eq and Show typeclasses are supported by this compiler at present
    * Type class constraints in type signatures (e.g. `Eq a =>` in `someFunc :: Eq a => a -> b -> Bool`) is not currently supported, but is expected to be implemented soon.
* **The IO a monad** (from [Prelude](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#t:IO)), specifically for type signatures, is not supported. 
    * Haskell type signatures that use the IO monad, e.g. the main function, must have an implicit type signature for proper translation to Lean
* **Language extensions**: This tool does not support features provided by [language extentions](https://hackage.haskell.org/package/template-haskell-2.23.0.0/docs/Language-Haskell-TH.html#g:5).
* **Library Imports**: This compiler does not yet support importing libraries. All programs must be supported exclusively by Haskell's built-in libraries.
* **Naming conflicts**: This compiler does not implement a renamer, and it is sensitive to naming conventions from the Haskell source code. This compiler is capable of creating rudimentary variable names that were implicitly typed in the original Haskell code, but it cannot identify or rename variables that conflict with Lean's syntax.
    * Data declarations cannot use names reserved by Lean's syntax
    * function names cannot begin with special characters (excluding `_`)
    * The compiler's variable name generation is alphabetical. Naming conflicts may occur when using an `a` as a universal `Type` identifier for user-declared types.

        For example: (in Haskell)
        ```
        myFunc :: MyType a -> String
        ``` 

        will produce
        ```
        def myFunc (a : MyType a) : String :=
        ```
    * **Unique Constructor Names**: Data declarations cannot have constructors with the same name as the data type. This will result in an error in the Lean translation because Lean would require extra context that would not be provided by the compiler.

* This tool has not yet been tested for modularity.

