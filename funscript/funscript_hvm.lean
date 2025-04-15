

structure Var where
  name: String
  type: String
  deriving Repr

inductive AST
| num : Nat -> AST
| lam : Var -> AST -> AST
| app : AST -> AST -> AST
| var : Var -> AST
deriving Repr

open AST

def prog :=
  let x := {name := "x", type := "Int"}
  let v := var x
  v

#eval prog
