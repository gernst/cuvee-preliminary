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

    case _: Id =>
      error("unknown identifier", expr, ty)

    case App(id, args) if (st.funs contains id) =>
      val ts1 = args map (infer(_, ty, st))
      val (ts2, tr) = st funs id
      ensure(ts1 == ts2, "arguments do not match function signature", id, ts1, ts2)
      tr

    case App(id, _) =>
      error("unknown function", id, expr, ty, st)

    case Bind(quant, formals, body) =>
      val ts: List[(Id, Type)] = formals
      val tr = infer(body, ty ++ ts, st)
      ensure(tr == Sort.bool, "body of bind must be boolean", body, tr)
      tr

    case Old(expr) => infer(expr, ty, st)

    case Eq(left, right) =>
      ensure(infer(left, ty, st) == infer(right, ty, st), "types of sides of equals must match")
      Sort.bool

    case Distinct(exprs) =>
      val types = exprs.map(infer(_, ty, st)).distinct
      ensure(types.length == 1, "types of arguments of distinct must match")
      Sort.bool

    case Ite(test, left, right) =>
      ensure(infer(test, ty, st) == Sort.bool, "test of if-then-else expression must be boolean", test)
      val lt = infer(left, ty, st)
      val rt = infer(right, ty, st)
      ensure(lt == rt, "types of then- and else-clause must match", left, lt, right, rt)
      lt

    case WP(prog, post) => inferWpLike(prog, post, ty, st)
    case Box(prog, post) => inferWpLike(prog, post, ty, st)
    case Dia(prog, post) => inferWpLike(prog, post, ty, st)

    case Let(pairs) => ???
    case Match(expr, cases) => ???
    case Select(array, index) => ???
    case Store(array, index, value) => ???
  }

  def inferWpLike(prog: Prog, post: Expr, ty: Map[Id, Type], st: State): Type = {
    ensure(infer(post, ty, st) == Sort.bool, "post-condition must be boolean", post)
    checkProg(prog, ty, st)
    Sort.bool
  }

  def checkProg(prog: Prog, ty: Map[Id, Type], st: State): Unit = prog match {
    case Block(progs, withOld) =>
      for (prog <- progs)
        checkProg(prog, ty, st)

    case Break =>
    // nothing to check

    case Assign(pairs) =>
      for (Pair(id, value) <- pairs) {
        val expected = ty.getOrElse(id, () => error("undefined variable", id))
        val actual = infer(value, ty, st)
        ensure(expected == actual, "value type must match variable type in assignment", id, value)
      }

    case Spec(xs, pre, post) =>
      val vars: Map[Id, Type] = (xs map (id => id -> infer(id, ty, st))).toMap
      ensure(infer(pre, ty ++ vars, st) == Sort.bool, "pre-condition must be boolean", pre)
      ensure(infer(post, ty ++ vars, st) == Sort.bool, "post-condition must be boolean", pre)

    case If(test, left, right) =>
      ensure(infer(test, ty, st) == Sort.bool, "test of if-then-else statement must be boolean", test)
      checkProg(left, ty, st)
      checkProg(right, ty, st)

    case While(test, body, after, term, pre, post) =>
      ensure(infer(test, ty, st) == Sort.bool, "test of while statement must be boolean", test)
      checkProg(body, ty, st)
      checkProg(after, ty, st)
      ensure(infer(term, ty, st) == Sort.int, "termination expression must be integral", term)
      ensure(infer(pre, ty, st) == Sort.bool, "pre-condition must be boolean", pre)
      ensure(infer(post, ty, st) == Sort.bool, "post-condition must be boolean", pre)

    case Call(name, in, out) if st.procs contains name =>
      val proc = st.procs(name)
      val args = in map (infer(_, ty, st))
      val ass = out map (infer(_, ty, st))
      ensure(proc._1 == args, "procedure call arguments do not math function signature", args, proc._1)
      ensure(proc._2 == ass, "procedure return values do not math function signature", args, proc._2)

    case Call(name, _, _) =>
      error("call to undefined procedure", name)
  }
}
