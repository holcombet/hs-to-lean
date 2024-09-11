def myMax (a b : Nat) : Nat :=
if a > b then a else b

def insert (a : Nat) (l : List Nat) : List Nat :=
if l.isEmpty then [a] else
if a <= l.head! then a :: l else
l.head! :: (insert a l.tail!)

def main : IO Unit := IO.println (myMax 10 20)
