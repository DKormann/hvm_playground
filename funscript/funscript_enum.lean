
inductive OptionTy (a : Type) : Type
| none : OptionTy a
| some : a → OptionTy a

inductive Ty : Type
| nat : Ty
| str : Ty
| option : Ty → Ty



inductive Expr : Ty → Type
| natLit : Nat → Expr Ty.nat
| strLit : String → Expr Ty.str
| var : String → {t : Ty} → Expr t
| none {a : Ty} : Expr (Ty.option a)
| some {a : Ty} : Expr a → Expr (Ty.option a)
| matchOption
    {a r : Ty}
    (e : Expr (Ty.option a))
    (noneCase : Expr r)
    (someCase : Expr a → Expr r)
    : Expr r

#eval
  let nt := Expr.natLit 42
