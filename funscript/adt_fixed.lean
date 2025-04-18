


inductive Ty
| int
| string
| arrow: Ty-> Ty -> Ty
| option: Ty -> Ty
| list: Ty -> Ty
| tree: Ty -> Ty
open Ty

structure Var (t: Ty) where
name: String


mutual
  inductive Maybe: Ty → Type
  | none: Maybe t
  | some: (x: Expr t) → Maybe t

  inductive Ls: Ty → Type
  | nil: Ls t
  | cons: (x: Expr t) → (xs: Ls t) → Ls t

  inductive Tree: Ty → Type
  | leaf: (x: Expr t) → Tree t
  | node: (l: Tree t) → (r: Tree t) → Tree t

  inductive Expr : Ty → Type
  | var : (v : Var t) → Expr t
  | intlit : Nat → Expr int
  | stringlit : String → Expr string
  | lam {a b : Ty} (param : Var a) (body : Expr b) : Expr (arrow a b)
  | app {a b : Ty} (f : Expr (arrow a b)) (x : Expr a) : Expr b
  | some {a : Ty} (x : Expr a) : Expr (option a)
  | mmatch {a b : Ty} (x: Maybe a) (default: Expr b) (arm: Expr (arrow a b) ) : Expr b

  | lmatch {a b : Ty} (x: Ls a) (n: Expr b) (arm: Expr (arrow a (arrow (list a) b))) : Expr b
  | tmatch {a b : Ty} (x: Tree a) (lf: Expr $ arrow a b) (nd: Expr $ arrow (tree a) $ arrow (tree a) b) : Expr b
end
open Expr

class Matchable (r:Ty) (t: Type) where
  domatch :t -> Expr r

instance : Matchable (r:Ty) ((Maybe x) × (Expr r) × (Expr (arrow x r))) where
  domatch x := mmatch x.1 x.2.1 x.2.2

instance : Matchable (r:Ty) ((Ls x) × (Expr r) × (Expr (arrow x (arrow (list x) r)))) where
  domatch x := lmatch x.1 x.2.1 x.2.2

instance : Matchable (r:Ty) ((Tree x) × (Expr (arrow x r)) × (Expr (arrow (tree x) $ arrow (tree x) r))) where
  domatch x := tmatch x.1 x.2.1 x.2.2

def makeMatch {r:Ty} {t:Type} [Matchable r t] (x: t) : Expr r := Matchable.domatch x


-- notation "λ" x y => lam x y


def eval  (x:Expr r) := match x with
  | var v => v.name
  | intlit n => toString n
  | stringlit s => s
  | lam p b => s!"λ{p.name}. {eval b}"
  | _ => "not implemented"

#eval
  let x := ⟨"x"⟩
  let f:Expr (arrow int int) := lam x (var x)

  -- let somi : Expr (option int) := some (intlit 22)
  let somi := Maybe.some (intlit 22)

  let mmatchi : Expr int := (Matchable.domatch (somi, intlit 33, f))

  eval f
