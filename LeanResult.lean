inductive Shape where
| Circle (a : Float)
| Rectangle (a : Float) (b : Float)

open Shape



def area (a : Shape) : Float := 
match a with
| (Circle r) => 3.14159 * r ^ 2
| (Rectangle l w) => l * w


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

