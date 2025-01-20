# hs-to-lean

This is a project by Tally Holcombe for her Masters in Electrical Engineering and Computer Science Thesis at Chapman University.

**CURRENTLY UNDER CONSTRUCTION** \
This is an older version of the current hs-to-lean project, cleaned up for basic readability and executability. 

For the sake of my Semi-Formalization for Mathematics paper, this version is hard-coded to translate the HeapSort algorithm (src/HeapSort.hs). 


## Description

The overarching goal of this project is to successfully validate Haskell s ource code by translating it into the [Lean Theorem Prover](https://lean-lang.org/). 

It is inspired by the work of Antal Spector-Zabusky, Joachim Breitner, Christine Rizkallah, and Stephanie Weirich in their paper [Total Haskell is Reasonable Coq](https://arxiv.org/abs/1711.09286). Their compiler can also be found [here](https://github.com/plclub/hs-to-coq).



## Dependencies

This project requires the following packages: 
* ghc 9.4.8
* ghc-lib-parser
* syb
* filepath

## Compilation

In the parent directory (hs-to-lean), run the program 
```
cabal build
cabal run 
```

See **src/result.txt** for the Lean translation.

## Limitations

This version of hs-to-lean has many limitations, which is why the translated code is written to a .txt file instead of a .lean file. 

The following limitations are specific to the HeapSort.hs translation, but can be applied to any Haskell program.


constructors live in the namespace

### Using ADTs

In Haskell, constructors are implicit when using the data structure. In the case of HeapSort, for example, we defined the Tree structure as: 
```
data Tree = Nil | Node Int Tree Tree
```

When we wish to use this structure, we can simple use the data constructor, `Nil` or `Node`, such as during the insert function:
```
insert x Nil = Node x Nil Nil
```

We do not have to specify that the `Nil` and `Node` constructors are from the `Tree` data type. 

However, in Lean, the type, generally, needs to be declared with the constructor in dot notation. The following snippet is the same line of code as above, translated into Lean:
```
x, Tree.Nil  => Tree.Node x Tree.Nil Tree.Nil
```

At present, specifying the type of non-primitive, inductive data types (e.g. Lists, Trees, etc.) is not implemented, and does not translate properly into Lean. This will be resolved in the finalized version of this project.


### Type Inference

One of Haskell's key features is its ability for type inference, meaning that functions do not require an explicit type signature.

For example, the first snippet is a Haskell function *with* an explicit type signature, while the second snippet is implicitly typed:
```
heapSort :: [Int] -> [Int]
heapSort = tree2list . heapify
```

```
heapSort = tree2list . heapify
```

Lean is able to support implicit type signatures, but this compiler has not yet implemented them yet. At present, the compiler views implicitly typed functions as the body of a previous function. 

**Expected Lean Code:**
```
def heapSort := tree2list (heapify)
```
**Current Translation:**
```
| => tree2list . heapify
```

The dot, `.`, in Haskell is a functional composition operator. Lean also has a functional composition operator, $\circ$, that has not yet been implemented into this compiler. 


### Main function

This compiler does not support many of the actions often used in main functions (e.g. IO(), printing, liftIO, etc.). For this reason, the main function was removed from the HeapSort.hs source code. We hope that main functions will be fully supported in the final version
