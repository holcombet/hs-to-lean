abbrev Name  := Int

inductive Expr where
| Var (a : Name)
| Let (a : Name) (b : Expr) (c : Expr)
| Lam (a : Name) (b : Expr)
| App (a : Expr) (b : Expr)

open Expr



def maxName (n : Name) (ns : List Name) : Name := 
List.foldr max n ns



def maxVar (a : Expr) : Int := 
match a with
| (Var n) => n
| (Let n e1 e2) => maxName n [maxVar e1, maxVar e2]
| (Lam n e) => maxName n [maxVar e]
| (App e1 e2) => maxName (maxVar e1) [maxVar e2]


def fresh (e : Expr) : Int := 
1 + maxVar e



def anf (a : Expr) : Expr := 
match a with
| (Var n) => Var n
| (Let n rhs body) => Let n (anf rhs) (anf body)
| (Lam n body) => Lam n (anf body)
| (App f (Var n)) => App (anf f) (Var n)
| (App f e) => let v  := fresh f
(Let v (anf e) (App (anf f) (Var v)))
