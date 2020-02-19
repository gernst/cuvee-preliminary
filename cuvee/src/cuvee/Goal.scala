package cuvee

object Goal {
  val empty = Goal(Nil, Nil, Nil, Nil)

  def apply(phis: List[Expr]): Goal = {
    empty assume phis
  }
}

case class Goal(bound: List[Formal], asserts: List[Expr], concls: List[Expr], goals: List[Goal]) {
  def isClosed: Boolean = {
    concls.isEmpty && goals.forall(_.isClosed)
  }

  def debug(indent: String) {
    if (bound.nonEmpty)
      println(indent + "fix")
    for (Formal(id, typ) <- bound) {
      println(indent + "  " + id + ": " + typ)
    }

    if (asserts.nonEmpty)
      println(indent + "assume")
    for (phi <- asserts) {
      println(indent + "  " + phi)
    }

    if (concls.nonEmpty)
      println(indent + "show")
    for (phi <- concls) {
      println(indent + "  " + phi)
    }

    for (goal <- goals) {
      println(indent + "subgoal")
      goal.debug(indent + "  ")
    }
  }

  def assume(phi: List[Expr]): Goal = {
    phi.foldLeft(this)(_ assume _)
  }

  def assert(phi: List[Expr]): Goal = {
    phi.foldLeft(this)(_ assert _)
  }

  def bind(formals: List[Formal]): Goal = {
    copy(bound = bound ++ formals)
  }

  def assume(phi: Expr): Goal = phi match {
    //    case Not(arg) =>
    //      assert(arg)
    case And.nary(args) =>
      assume(args)
    case Or.nary(args) =>
      val extra = args map Goal.empty.assume
      copy(goals = extra ++ goals)
    /* case Imp(phi, psi) =>
      val goal1 = Goal.empty assert phi
      val goal2 = Goal.empty assume phi assert psi
      copy(goals = goal1 :: goal2 :: goals) */
    /* case expr @ Exists(_, _) =>
      val Bind(_, formals, body) = expr.refresh
      this bind formals assume body */
    /* case Forall(formals, body) =>
      val goal = Goal.empty bind formals assert body
      Goal(bound, goal :: sub, ant, suc) */
    case _ =>
      copy(asserts = phi :: asserts)
  }

  def assert(phi: Expr): Goal = phi match {
    //    case Not(arg) =>
    //      assume(arg)
    case And.nary(args) =>
      /* val extra = args map Goal.empty.assert
      copy(goals = extra ++ goals) */
      assert(args)
    case Or.nary(args) =>
      val ant = Not(args.init)
      val suc = args.last
      this assume ant assert suc
    case Imp(ant, suc) =>
      val goal = Goal.empty assume ant assert suc
      copy(goals = goal :: goals)
    case Forall(bound, body) =>
      val goal = Goal.empty bind bound assert body
      copy(goals = goal :: goals)
    /* case Exists(formals, body) =>
      val goal = Goal.empty bind formals assert !body
      Goal(bound, goal :: sub, ant, suc) */
    case _ =>
      copy(concls = phi :: concls)
  }
}
