inductive AST
| num : Nat → AST
| add : AST → AST → AST
deriving Repr

open AST

instance : HAdd Nat Nat AST where
  hAdd a b := add (num a) (num b)

#eval (1 + 2 : AST)  -- OK: uses your instance, output: add (num 1) (num 2)
