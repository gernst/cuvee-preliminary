package cuvee

trait Proof {
  def toExpr: Expr
}

object Goal {
  val empty = Goal(Nil, Nil)

  def assume(expr: Expr): Goal = {
    empty assume expr
  }

  def assert(expr: Expr): Goal = {
    empty assert expr
  }

  def apply(exprs: List[Expr]): Goal = {
    empty assume exprs
  }
}

case class Pos(phi: Expr) extends Proof {
  def toExpr = phi
  override def toString = phi.toString
}

case class Neg(phi: Expr) extends Proof {
  def toExpr = !phi
  override def toString = sexpr("not", phi)
}

case class Cases(cases: List[Proof]) extends Proof {
  def toExpr = Or(cases map (_.toExpr))
  override def toString = sexpr("or", cases: _*)
}

object Cases {
  def assume(exprs: List[Expr]) = {
    Cases(exprs map Goal.assume)
  }

  def assert(exprs: List[Expr]) = {
    Cases(exprs map Goal.assert)
  }
}

case class Goal(scope: List[Formal], props: List[Proof]) extends Proof {
  override def toString = sexpr("forall", sexpr(scope), sexpr("and", props: _*))
  def toExpr = Forall(scope, And(props map (_.toExpr)))

  def assume(phi: List[Expr]): Goal = {
    phi.foldLeft(this)(_ assume _)
  }

  def assert(phi: List[Expr]): Goal = {
    phi.foldLeft(this)(_ assert _)
  }

  def bind(formals: List[Formal]): Goal = {
    Goal(scope ++ formals, props)
  }

  def assume(phi: Expr): Goal = phi match {
    case App(Id.not, List(arg)) =>
      assert(arg)
    case App(Id.and, args) =>
      assume(args)
    case App(Id.or, args) =>
      Goal(scope, Cases.assume(args) :: props)
    case App(Id.imp, List(ant, suc)) =>
      val args = List(!ant, suc)
      Goal(scope, Cases.assume(args) :: props)
    case expr @ Exists(_, _) =>
      val Bind(_, formals, body) = expr.refresh
      bind(formals).assume(body)
    case _ =>
      Goal(scope, Pos(phi) :: props)
  }

  def assert(phi: Expr): Goal = phi match {
    case App(Id.not, List(arg)) =>
      assume(arg)
    case App(Id.and, args) =>
      Goal(scope, Cases.assert(args) :: props)
    case App(Id.or, args) =>
      assert(args)
    case App(Id.imp, List(ant, suc)) =>
      assume(ant).assert(suc)
    case expr @ Forall(_, _) =>
      val Bind(_, formals, body) = expr.refresh
      bind(formals).assert(body)
    case _ =>
      Goal(scope, Neg(phi) :: props)
  }
}
