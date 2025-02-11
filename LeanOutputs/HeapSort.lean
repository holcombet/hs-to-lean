inductive Tree where
| Nil 
| Node (a : Int) (b : Tree) (c : Tree)

open Tree



def insert (x : Int) (a : Tree) : Tree := 
match x, a with
| x, Nil  => Node x Nil Nil
| x, (Node a left right) => if x < a then Node a (insert x left) right else Node a left (insert x right)


def heapify (xs : List Int) : Tree := 
match xs with
| []  => Nil
| (x :: xs) => insert x (heapify xs)


def tree2list (a : Tree) : List Int := 
match a with
| Nil  => []
| (Node a left right) => tree2list left ++ [a] ++ tree2list right


def heapSort  := tree2list ∘ heapify



def main  := do
    IO.print $ heapSort [5, 2, 8, 1, 3, 7, 9]
    IO.print $ heapSort [9, 7, 3, 1, 8, 2, 5]


