def myMax (a b : Nat) : Nat :=
  if a > b then a else b

def myInsert (a : Nat) : (List Nat) -> List Nat
| [] => [a]
| x :: xs => if a <= x
                then a :: x :: xs
                else x :: myInsert a xs

def main : IO Unit := IO.println (myInsert 2 [1,3])
