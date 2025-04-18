


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


inductive Expr : Ty → Type
| var : (v : Var t) → Expr t
| intlit : Nat → Expr int
| stringlit : String → Expr string
| lam {a b : Ty} (param : Var a) (body : Expr b) : Expr (arrow a b)
| app {a b : Ty} (f : Expr (arrow a b)) (x : Expr a) : Expr b
| none : Expr (option a)
| some  (x : Expr a) : Expr (option a)
| nil : Expr (list a)
| cons  (x : Expr a) (xs : Expr (list a)) : Expr (list a)
| leaf (x: Expr a) : Expr $ tree a
| node (l: Expr $ tree a) (r: Expr $ tree a) : Expr $ tree a
| mmatch {a b : Ty} (x: Expr (option a)) (dft: Expr b) (bdr: Var a) (bod: Expr b) : Expr b
| lmatch {a b : Ty} (x: Expr $ list a) (n: Expr b) (h: Var a) (t: Var $ list a) (bod: Expr b): Expr b
| tmatch {a b : Ty} (x: Expr $ tree a) (lfb: Var a) (r: Expr b) (ln rn: Var $ tree a) (bod: Expr b): Expr b


open Expr

class Matchable (r:Ty) (t: Type) where
  domatch :t -> Expr r

instance {a b} : Matchable (b:Ty) ((Expr $ option a) × (Expr b) × (Var a) × (Expr b)) where
  domatch := λ ⟨x, d, v, e⟩ => mmatch x d v e

instance {a b} : Matchable (b:Ty) ((Expr $ list a) × (Expr b) × ((Var a) × (Var $ list a)) × (Expr b)) where
  domatch := λ (x, d, (va, vb), e) => lmatch x d va vb e

instance {a b} : Matchable (b:Ty) ((Expr $ tree a) × ((Var a) × (Expr b)) × ((Var $ tree a) × (Var $ tree a)) × (Expr b)) where
  domatch := λ (x, (va, vb), (la, ra), e) => tmatch x va vb la ra e

def makeMatch {r:Ty} {t:Type} [Matchable r t] (x: t) : Expr r := Matchable.domatch x


class ToExpr (t: Type) (b:Ty) where
  toExpr : t → Expr b
instance {b} : ToExpr (Expr b) b where
  toExpr e := e
instance : ToExpr Nat int where toExpr n := intlit n
instance : ToExpr String string where toExpr n := stringlit n
instance {a b} [ToExpr a b] : ToExpr (Option a) (option b) where toExpr o := match o with
  | Option.none => none
  | Option.some x => Expr.some (ToExpr.toExpr x)
def lExpr {a b} [ToExpr a b] (l:List a) : Expr (list b) := match l with
  | [] => Expr.nil
  | x::xs => Expr.cons (ToExpr.toExpr x) (lExpr xs)
instance [ToExpr a b] : ToExpr (List a) (Ty.list b) where toExpr o := (lExpr o)

open ToExpr


def g : Expr int := ToExpr.toExpr 22
def og: Expr (option int) := ToExpr.toExpr (some 22)
def lg: Expr (list int) := ToExpr.toExpr [22, 33, 44]


def eval  (x:Expr r) := match x with
  | var v => v.name
  | intlit n => toString n
  | stringlit s => s
  | lam p b => s!"λ{p.name}. {eval b}"
  | app f x => s!"({eval f} {eval x})"
  | Expr.none => "none"
  | Expr.some x => s!"some {eval x}"
  | mmatch i d b x => s!"~({eval i}) \{ #None:{22} #Some\{{b.name}}: {eval x}}"
  | _ => "not implemented"

#eval
  let x: Var int := ⟨"x"⟩

  let somi: Expr (option int) := some (intlit 22)

  let mmatchi : Expr int := (Matchable.domatch (somi, intlit 33, x, var x))

  let lsi :Expr (list int) := (toExpr [22, 33, 44])



  eval (lam x mmatchi)
