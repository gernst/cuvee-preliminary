package cuvee

object Normalize {
  def alpha(phi: Expr): Expr = phi match {
    case And.nary(args) =>
      And.nary(args map alpha)
    case Or.nary(args) =>
      Or.nary(args map alpha)
    case Imp(phi, psi) =>
      alpha(phi) ==> alpha(psi)
    case Not(phi) =>
      !alpha(phi)
    case bind: Bind =>
      val Bind(quant, formals, body) = bind.refresh
      Bind(quant, formals, alpha(body))
  }

  def nnf(phi: Expr): Expr = phi match {
    case And.nary(args) =>
      And.nary(args map nnf)
    case Or.nary(args) =>
      Or.nary(args map nnf)
    case Imp(phi, psi) =>
      nnf(!phi || psi)
    case Bind(quant, formals, body) =>
      Bind(quant, formals, nnf(body))
    case Not(Not(phi)) =>
      nnf(phi)
    case Not(Distinct(List(left, right))) =>
      Eq(left, right)
    case Not(Eq(left, right)) =>
      Distinct(List(left, right))
    case Not(App(Id.lt, args)) =>
      App(Id.ge, args)
    case Not(App(Id.le, args)) =>
      App(Id.gt, args)
    case Not(App(Id.gt, args)) =>
      App(Id.le, args)
    case Not(App(Id.ge, args)) =>
      App(Id.lt, args)
    case Not(And.nary(args)) =>
      Or.nary(args map (arg => nnf(!arg)))
    case Not(Or.nary(args)) =>
      And.nary(args map (arg => nnf(!arg)))
    case Not(Imp(phi, psi)) =>
      nnf(phi && !psi)
    case Not(Bind(quant, formals, body)) =>
      Bind(!quant, formals, nnf(!body))
    case _ =>
      phi
  }
}