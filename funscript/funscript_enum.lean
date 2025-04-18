
-- superposed lambda calculus DSL

inductive Ty
| int : Ty
| string : Ty
| arrow : Ty → Ty → Ty
-- | list : Ty → Ty
-- | maybe : Ty → Ty
deriving Repr

open Ty

structure Var (t: Ty) where
  name: String
deriving Repr

structure Variant (t: Ty) where
  name: String
deriving Repr

inductive Data: (List (Variant t)) -> Type
| nil: Data []
| cons {t} (v: Variant t) (vs: List (Variant t)) : Data (v::vs)

inductive Matcher : (List (Variant t)) -> Type
| nil: (Matcher [])
| cons {t} (v: Variant t) (vs: List (Variant t)) : Matcher (v::vs)

inductive Expr: Ty -> Type
| var: (v : Var t) -> Expr t
| intlit : Nat -> Expr int
| stringlit : String -> Expr string

| matcharm: (Variant v -> Expr t) -> Expr t
| matcher {t}: (vs: MatchArm  v t) -> Expr t
| lam {a b : Ty} (param: Var a) (body: Expr b) : Expr (arrow a b)
| app {a b : Ty} (f: Expr (arrow a b)) (x: Expr a) : Expr b
| sup {a:Ty} (label: Nat) (x: Expr a) (b: Expr a) : Expr a
| dup {a b:Ty} (label: Nat) (x y: Var a) (z: Expr a) (r: Expr b): Expr b

open Expr


class ToExpr (t: Type) (b:Ty) where
  toExpr: t → Expr b

instance {b} : ToExpr (Expr b) b where
  toExpr e := e


instance: ToExpr Nat int where toExpr n := intlit n
instance: ToExpr String string where toExpr n := stringlit n



def useExpr {t:Type} {b:Ty} [ToExpr t b] (x: t): Expr b := ToExpr.toExpr x



def newvar {t:Ty} (x: String): Expr t := var ⟨x⟩


def compile: Expr t -> String
| var v => v.name
| intlit n => toString n
| stringlit s => s
| lam param body =>
  s!"λ{param.name}. {compile body}"
| app f x =>
  s!"({compile f} {compile x})"
| sup label x b =>
  s!"&{label}\{{compile x} {compile b}}"
| dup label x y z r =>
  s!"!&{label}\{{x.name} {y.name}}={compile z}\n{compile r}"


#eval
  let x : Var int := ⟨"x"⟩
  let varx := var x

  -- let l: Var
  let n :Var int := ⟨"n"⟩
  let c :Var int := ⟨"c"⟩

  let nil := lam n (lam c (var n))

  let i: Expr int := useExpr 22
  let j: Expr int := useExpr $ intlit 22

  let y : Expr int := newvar "y"

  let x2 := ⟨"x2"⟩
  let f : Expr (arrow int int) := lam x (var x)
  let f2 :Expr (arrow int int) := lam ⟨"y"⟩ (var x2)

  let res := app f (var x)
  let s: Expr int  := sup 0 (var x) res

  let supres := app f s


  compile supres
