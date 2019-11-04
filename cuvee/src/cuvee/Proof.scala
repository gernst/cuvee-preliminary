package cuvee

trait Proof {
}

object Proof {
  val empty = Goal(Nil, Nil)

  def assume(expr: Expr): Proof = {
    empty assume expr
  }

  def assert(expr: Expr): Proof = {
    empty assert expr
  }
}

case class Pos(phi: Expr) extends Proof {
  override def toString = phi.toString
}

case class Neg(phi: Expr) extends Proof {
  override def toString = sexpr("not", phi)
}

case class Cases(cases: List[Proof]) extends Proof {
  override def toString = sexpr("or", cases)
}

object Cases {
  def assume(exprs: List[Expr]) = {
    Cases(exprs map Proof.assume)
  }

  def assert(exprs: List[Expr]) = {
    Cases(exprs map Proof.assert)
  }
}

case class Goal(scope: List[Formal], props: List[Proof]) extends Proof {
  override def toString = sexpr("forall", scope, sexpr("and", props))
    
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
