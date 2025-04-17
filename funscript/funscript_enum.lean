

inductive Ty
| int : Ty
| bool : Ty
| string : Ty
| arrow : Ty → Ty → Ty

deriving Repr

open Ty


structure EnumVariant where
  name : String
  fields : List Ty
deriving Repr

structure Enum where
  name : String
  variants : List EnumVariant
deriving Repr


structure Var (t: Ty) where
  name: String
deriving Repr


inductive Expr: Ty -> Type
| var: (v : Var t) -> Expr t
| lam {a b : Ty} (param: Var a) (body: Expr b) : Expr (arrow a b)
| app {a b : Ty} (f: Expr (arrow a b)) (x: Expr a) : Expr b
| sup {a:Ty} (label: Nat) (x: Expr a) (b: Expr a) : Expr a
| dup {a b:Ty} (label: Nat) (x y: Var a) (z: Expr a) (r: Expr b): Expr b
| match {e:Enum} {a:Ty} (x: Expr)

deriving Repr

open Expr


def newvar {t:Ty} (x: String): Expr t := var ⟨x⟩

def compile: Expr t -> String
| var v => v.name
| lam param body =>
  s!"λ{param.name}. {compile body}"
| app f x =>
  s!"({compile f} {compile x})"
| sup label x b =>
  s!"&{label}\{{compile x} {compile b}}"
| dup label x y z r =>
  s!"!&{label}\{{x.name} {y.name}}={compile z}\n{compile r}"
