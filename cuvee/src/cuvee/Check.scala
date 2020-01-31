package cuvee

object Check {
  def infer(expr: Expr, ty: Map[Id, Type], st: State): Type = expr match {
    case _: Num =>
      Sort.int

    case id: Id if (ty contains id) =>
      ty(id)

    case id: Id if (st.funs contains id) =>
      val (ts, tr) = st funs id
      ensure(ts.isEmpty, "not constant", expr, ty)
      tr

    case id: Id =>
      error("unknown identifier", expr, ty)

    case App(id, args) if (st.funs contains id) =>
      val ts1 = args map (infer(_, ty, st))
      val (ts2, tr) = st funs id
      ensure(ts1 == ts2)
      tr

    case App(id, args) =>
      error("unknown function", id, expr, ty, st)

    case Bind(quant, formals, body) =>
      val ts: List[(Id, Type)] = formals
      val tr = infer(body, ty ++ ts, st)
      ensure(tr == Sort.bool)
      tr
  }
}