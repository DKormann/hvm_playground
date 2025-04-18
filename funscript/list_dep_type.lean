

inductive Ty
| int
| string
| arrow: Ty-> Ty -> Ty
| adt: String -> List Ty -> Ty
open Ty


structure Var (t: Ty) where
name: String

inductive Expr: Ty -> Type
| var: (v : Var t) -> Expr t
| intlit : Nat -> Expr int
| stringlit : String -> Expr string
| lam {a b : Ty} (param: Var a) (body: Expr b) : Expr (arrow a b)
| app {a b : Ty} (f: Expr (arrow a b)) (x: Expr a) : Expr b
open Expr


#eval








  22
