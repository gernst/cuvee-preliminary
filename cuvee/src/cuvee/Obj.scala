package cuvee

case class Body(locals: List[Formal], progs: List[Prog]) {
  def bound = Set(locals map (_.id): _*)

  def prog = progs match {
    case Nil => Skip
    case List(prog) => prog
    case _ => Block(progs)
  }

  def mod = Set(progs flatMap (_.mod): _*) -- bound

  def replace(re: Map[Id, Id]) = {
    val xs = Expr.free(re)
    val captured = bound & xs
    val a = Expr.fresh(captured)
    Body(locals map (_ rename a), progs map (_ replace (re -- bound ++ a)))
  }

  override def toString = {
    if (locals.isEmpty) sexpr(progs)
    else sexpr(sexpr("let", locals), progs)
  }
}

case class Proc(in: List[Formal], out: List[Formal], pre: Expr, post: Expr, body: Option[Body]) {
  def sig = (in map (_.typ), out map (_.typ))

  def call(ps: List[Formal]): (List[Formal], List[Formal], Expr, Expr, Body) = {
    call(ps, ps)
  }

  def call(ps: List[Formal], xs: List[Id]): (List[Formal], List[Formal], Expr, Expr, Body) = {
    val (pre, post, body) = call(ps, xs, in, out)
    (in, out, pre, post, body)
  }

  def call(ps: List[Formal], xi: List[Id], xo: List[Id]): (Expr, Expr, Body) = {
    call(ps, ps, xi, xo)
  }

  def call(ps: List[Formal], xs: List[Id], xi: List[Id], xo: List[Id]): (Expr, Expr, Body) = {
    val formals = ps ++ in ++ out
    val args = xs ++ xi ++ xo
    val re = Expr.subst(formals, args)
    val _body = unwrap(body, "cannot call procedure without implementation", body)
    (pre rename re, post rename re, _body replace re)
  }

  override def toString = Printer.proc(in, out, pre, post, body)
}

object Proc extends ((List[Formal], List[Formal], Option[Expr], Option[Expr], Option[Body]) => Proc) {
  def apply(in: List[Formal], out: List[Formal], pre: Option[Expr], post: Option[Expr], body: Option[Body]): Proc = {
    val _pre = pre.getOrElse(True)
    val _post = post.getOrElse(True)
    Proc(in, out, _pre, _post, body)
  }

  def apply(in: List[Formal], out: List[Formal], pre: Expr, post: Expr, body: Prog*): Proc = {
    Proc(in, out, pre, post, Some(Body(Nil, body.toList)))
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