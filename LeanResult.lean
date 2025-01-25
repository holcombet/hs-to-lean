def frac  : Float := 
1 / 2



abbrev Name  := String

abbrev ResultFunction a b := a -> Except String b

inductive Something (a : Type) (b : Type) where
| Blah (c : a)
| Bleh (c : b)

inductive Color where
| Red 
| Green 
| Blue 

inductive Tree (a : Type) where
| Empty 
| Node (b : a) (c : (Tree a)) (d : (Tree a))

inductive Tree2 where
| Nil 
| Nod (a : Int) (b : Tree2) (c : Tree2)

def add (a : Int) (b : Int) : Int := 
a + b



def abc  : Int := 
13



def random (x : Int) (ys : List Int) : Int := 
match x, ys with
| x, [] => x
| x, (y :: ys) => x - 1


def categorizeNumber (x : Int) : String := 
if x < 0 then "Negative" else
if x == 0 then "Zero" else
if x < 10 then "Small" else
"Large"



def calculateArea (r : Float) : Float := 
let pi := 3.14
(pi * r * r)



def calculateRandom (x : Int) : Int := 
let y := 10
let z := 2
(x + y + z)



def nthElement (xs : List a) (b : Int) : Option a := 
match xs, b with
| [], b => none
| (x :: xs), b => 
if b <= 0 then none else

if b == 1 then some x else
nthElement xs (b - 1)

def insert (x : Int) (ys : List Int) : List Int := 
match x, ys with
| x, [] => [x]
| x, (y :: ys) => if x < y then x :: y :: ys else y :: insert x ys


def insertionSort (xs : List Int) : List Int := 
match xs with
| [] => []
| (x :: xs) => insert x (insertionSort xs)


def applyFunc (f : (Int -> Int)) (x : Int) : Int := 
f x



def main  := IO.print "Hello World"

