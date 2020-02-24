package cuvee

case class Proc(in: List[Formal], out: List[Formal], pre: Expr, post: Expr, body: Prog) {
  def sig = (in map (_.typ), out map (_.typ))

  def call(ps: List[Formal]): (List[Formal], List[Formal], Expr, Expr, Prog) = {
    call(ps, ps)
  }

  def call(ps: List[Formal], xs: List[Id]): (List[Formal], List[Formal], Expr, Expr, Prog) = {
    val (pre, post, body) = call(ps, xs, in, out)
    (in, out, pre, post, body)
  }

  def call(ps: List[Formal], xi: List[Id], xo: List[Id]): (Expr, Expr, Prog) = {
    call(ps, ps, xi, xo)
  }

  def call(ps: List[Formal], xs: List[Id], xi: List[Id], xo: List[Id]): (Expr, Expr, Prog) = {
    val formals = ps ++ in ++ out
    val args = xs ++ xi ++ xo
    val re = Expr.subst(formals, args)
    (pre rename re, post rename re, body replace re)
  }

  override def toString = Printer.proc(in, out, pre, post, body)
}

object Proc extends ((List[Formal], List[Formal], Prog, Option[Expr], Option[Expr]) => Proc) {
  def apply(in: List[Formal], out: List[Formal], body: Prog, pre: Option[Expr], post: Option[Expr]): Proc = {
    val _pre = pre.getOrElse(True)
    val _post = post.getOrElse(True)
    Proc(in, out, _pre, _post, body)
  }
}

case class Obj(state: List[Formal], init: Proc, ops: List[(Id, Proc)]) {
  def op(id: Id) = id match {
    case Id.init =>
      init
    case _ =>
      val Some((_, proc)) = ops find (_._1 == id)
      proc
  }
}