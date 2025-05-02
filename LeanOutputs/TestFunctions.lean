def frac   : Float := 
1 / 2



abbrev Name  := String

abbrev Result a b := a -> Except String b

abbrev Transform  := Int -> Int

abbrev Numbers  := List Int

def sumNumbers  (xs : Numbers) : Int := 
match xs with
| []  => 0
| (x :: xs) => x + sumNumbers xs


inductive Something (a : Type) (b : Type) where
| Blah (c : a)
| Bleh (c : b)

open Something



inductive Color where
| Red 
| Green 
| Blue 

open Color



inductive Tree (a : Type) where
| Empty 
| Node (b : a) (c : (Tree a)) (d : (Tree a))

open Tree



inductive Tree2 where
| Nil 
| Nod (a : Int) (b : Tree2) (c : Tree2)

open Tree2



def add  (a : Int) (b : Int) : Int := 
a + b



def abc   : Int := 
13



def random  (x : Int) (ys : List Int) : Int := 
match x, ys with
| x, []  => x
| x, (y :: ys) => x - 1


def categorizeNumber  (x : Int) : String := 
if x < 0 then "Negative" else
if x == 0 then "Zero" else
"Positive"



def calculateArea  (r : Float) : Float := 
let pi  := 3.14
(3.14159 * r * r)



def calculateRandom  (x : Int) : Int := 
let y  := 10
let z  := 2
(x + y + z)



def nthElement  (xs : List a) (b : Int) : Option a := 
match xs, b with
| [] , b => none
| (x :: xs), b => 
if b <= 0 then none else

if b == 1 then some x else
nthElement xs (b - 1)

def insert  (x : Int) (ys : List Int) : List Int := 
match x, ys with
| x, []  => [x]
| x, (y :: ys) => if x < y then x :: y :: ys else y :: insert x ys


def insertionSort  (xs : List Int) : List Int := 
match xs with
| []  => []
| (x :: xs) => insert x (insertionSort xs)


def applyFunc  (f : (Int -> Int)) (x : Int) : Int := 
f x



def circleArea  (radius : Float) : Float := 
piVal * radiusSquared
where
piVal  := 3.14159

radiusSquared  := radius * radius




def pad  (day : Int) : String := 
match (toString day).data with
| [c]=> String.mk ([' ', c])
| cs=> String.mk (cs)




def printList [Repr a] (xs : List a) : IO Unit := 
match xs with
| []  => IO.println "Empty list"
| (x :: xs) => do
    IO.println (repr x)
    printList xs



inductive DayOfWeek where
| Sunday 
| Monday 
| Tuesday 
| Wednesday 
| Thursday 
| Friday 
| Saturday 
deriving Repr, DecidableEq

open DayOfWeek



def main   : IO Unit := 
do
    IO.print $ calculateArea 2
    IO.println "Hello World!"
    IO.print $ calculateRandom 2


