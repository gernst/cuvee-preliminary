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

    case Note(expr, _) =>
      infer(expr, ty, st)

    case App(id, args) if (st.funs contains id) =>
      val ts1 = args map (infer(_, ty, st))
      val (ts2, tr) = st funs id
      ensure(ts1 == ts2, "arguments do not match function signature", id, ts1, ts2)
      tr

    case App(id, _) =>
      error("unknown function", id, expr, ty, st)

    case Bind(_, formals, body) =>
      val ts: List[(Id, Type)] = formals
      val tr = infer(body, ty ++ ts, st)
      ensure(tr == Sort.bool, "body of bind must be boolean", body, tr)
      tr

    case Old(expr) =>
      infer(expr, ty, st)

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

    case Let(pairs, body) =>
      val vars = pairs.map(pair => pair.x -> infer(pair.e, ty, st)).toMap
      infer(body, ty ++ vars, st)

    case Match(expr, cases) => ???
    case Select(array, index) => ???
    case Store(array, index, value) => ???
  }

  private def inferWpLike(prog: Prog, post: Expr, ty: Map[Id, Type], st: State): Type = {
    ensure(infer(post, ty, st) == Sort.bool, "post-condition must be boolean", post)
    checkProg(prog, ty, st, false)
    Sort.bool
  }

  /**
   * @param loop true if this program is in a loop. This allows the break statement.
   */
  def checkProg(prog: Prog, ty: Map[Id, Type], st: State, loop: Boolean): Unit = prog match {
    case Block(progs, withOld) =>
      for (prog <- progs)
        checkProg(prog, ty, st, loop)

    case Break =>
      ensure(loop, "break most only occur in while")

    case Assign(pairs) =>
      for (Pair(id, value) <- pairs) {
        val expected = ty.getOrElse(id, () => error("undefined variable", id))
        val actual = infer(value, ty, st)
        ensure(expected == actual, "value type must match variable type in assignment", id, value)
      }

    case Spec(xs, pre, post) =>
      xs.foreach(infer(_, ty, st)) // just check if defined
      ensure(infer(pre, ty, st) == Sort.bool, "pre-condition must be boolean", pre)
      ensure(infer(post, ty, st) == Sort.bool, "post-condition must be boolean", pre)

    case If(test, left, right) =>
      ensure(infer(test, ty, st) == Sort.bool, "test of if-then-else statement must be boolean", test)
      checkProg(left, ty, st, loop)
      checkProg(right, ty, st, loop)

    case While(test, body, after, term, pre, post) =>
      ensure(infer(test, ty, st) == Sort.bool, "test of while statement must be boolean", test)
      checkProg(body, ty, st, true)
      checkProg(after, ty, st, loop)
      ensure(infer(term, ty, st) == Sort.int, "termination expression must be integral", term)
      ensure(infer(pre, ty, st) == Sort.bool, "pre-condition must be boolean", pre)
      ensure(infer(post, ty, st) == Sort.bool, "post-condition must be boolean", pre)

    case Call(name, in, out) if st.procs contains name =>
      val (xs, ys) = st.procs(name)
      val args = in map (infer(_, ty, st))
      val ass = out map (infer(_, ty, st))
      ensure(xs == args, "procedure call arguments do not math function signature", args, xs)
      ensure(ys == ass, "procedure return values do not math function signature", args, ys)

    case Call(name, _, _) =>
      error("call to undefined procedure", name)
  }
}
