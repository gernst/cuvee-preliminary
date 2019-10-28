package cuvee

trait Proof {

}

object Goal {
  val empty = Goal(Nil, Nil, Nil)

  def assume(expr: Expr): Goal = {
    Goal.empty assume expr
  }
  
  def assert(expr: Expr): Goal = {
    Goal.empty assert expr
  }
}

case class Step(prems: List[Proof], concl: Goal) extends Proof {

}

case class Goal(scope: List[Formal], ant: List[Expr], suc: List[Expr]) extends Proof {
  def assume(phi: List[Expr]): Goal = {
    phi.foldLeft(this)(_ assume _)
  }

  def assert(phi: List[Expr]): Goal = {
    phi.foldLeft(this)(_ assert _)
  }

  def bind(formals: List[Formal]): Goal = {
    Goal(scope ++ formals, ant, suc)
  }

  def assume(phi: Expr): Goal = phi match {
    case App(Id.not, List(arg)) =>
      assert(arg)
    case App(Id.and, args) =>
      assume(args)
    case expr @ Exists(_, _) =>
      val Bind(_, formals, body) = expr.refresh
      bind(formals).assume(body)
    case _ =>
      Goal(scope, phi :: ant, suc)
  }

  def assert(phi: Expr): Goal = phi match {
    case App(Id.not, List(arg)) =>
      assume(arg)
    case App(Id.or, args) =>
      assert(args)
    case App(Id.imp, List(ant, suc)) =>
      assume(ant).assert(suc)
    case expr @ Forall(_, _) =>
      val Bind(_, formals, body) = expr.refresh
      bind(formals).assert(body)
    case _ =>
      Goal(scope, ant, phi :: suc)
  }

  def toExpr = {
    Forall(scope, Imp(And(ant), Or(suc)))
  }
}
