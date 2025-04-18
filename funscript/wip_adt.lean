


inductive Ty
| int
| string
| arrow: Ty-> Ty -> Ty
| adt: List (String × (List Ty)) -> Ty
open Ty

-- abbrev variants := List (String × (List Ty))


structure Var (t: Ty) where
name: String

inductive Expr : Ty → Type
| var : (v : Var t) → Expr t
| intlit : Nat → Expr int
| stringlit : String → Expr string
| lam {a b : Ty} (param : Var a) (body : Expr b) : Expr (arrow a b)
| app {a b : Ty} (f : Expr (arrow a b)) (x : Expr a) : Expr b
| con {variants : List (String × List Ty)} :
    (ctor : String) →
    (args : List (Σ t : Ty, Expr t)) →
    (h : (ctor, args.map (fun x => x.1)) ∈ variants) →
    Expr (adt variants)


open Expr


#eval

  let someint := ("some", [int])
  let noneint := ("none", [])

  let intmaybe := adt [noneint, someint]

  let somex : Var intmaybe := ⟨"x"⟩

  -- let somei : Expr intmaybe := con someint (by simp)


  22
