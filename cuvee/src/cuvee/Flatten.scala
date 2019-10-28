package cuvee

object Flatten {
  def scoped(cmds: List[Cmd]) = {
    List(Push) ++ cmds ++ List(Pop)
  }

  def skolem(expr: Bind, pos: Boolean): List[Cmd] = {
    val Bind(_, formals, body) = expr.refresh
    val consts = formals map {
      case Formal(id, typ) => DeclareFun(id, Nil, typ)
    }
    scoped(consts ++ assert(body, pos))
  }

  def assert(expr: Expr, pos: Boolean): List[Cmd] = expr match {
    case App(Id.not, List(arg)) =>
      assert(arg, !pos)
    case App(Id.and, args) if pos =>
      args flatMap { assert(_, pos) }
    case App(Id.or, args) if !pos =>
      args flatMap { assert(_, pos) }
    case App(Id.imp, List(ant, suc)) if !pos =>
      scoped(assert(ant, !pos) ++ assert(suc, pos))
    case expr @ Exists(_, _) if pos =>
      skolem(expr, pos)
    case expr @ Forall(_, _) if !pos =>
      skolem(expr, pos)
    case _ =>
      if (pos) List(Assert(expr))
      else List(Assert(!expr))
  }

  def flatten(cmd: Cmd): List[Cmd] = cmd match {
    case Assert(expr) =>
      assert(expr, pos = true)
    case _ =>
      List(cmd)
  }
}