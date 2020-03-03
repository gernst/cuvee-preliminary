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

  def checkObj(sort: Sort, obj: Obj): Unit = {
    val Obj(state, init, ops) = obj
    Check.checkProc(Id.init, init, state)
    ops.foreach(proc => Check.checkProc(proc._1, proc._2, state))
  }

  /**
   * Does some basic checks on a procedure w.r.t. well-definedness. This exludes anything that requires knowledge about
   * the state.
   *
   * @param id    name of the procedure for error messages
   * @param state (optional) state of the surrounding object if this procedure is defined on an object
   */
  def checkProc(id: Id, proc: Proc, state: List[Formal] = Nil): Unit = {
    val Proc(in, out, pre, post, body) = proc
    val inVars: List[Id] = in
    val duplicateInputDeclarations = inVars.groupBy(identity).filter(_._2.size > 1)
    ensure(duplicateInputDeclarations.isEmpty, s"The method $id declares duplicate input parameters ${duplicateInputDeclarations.keys.mkString(", ")}")

    // the outputs may have the same variable name in multiple places if the type is equal.
    // outputs may overlap with inputs but, again, the type must be equal
    val nonUniqueAgruments = (in ++ out).groupBy(_.id).filter(_._2.map(_.typ).distinct.size > 1)
    ensure(nonUniqueAgruments.isEmpty, "The method $id declares non-unique type for argument ${nonUniqueAgruments.keys.mkString(", ")}")

    for (body <- body) {
      checkBody(id, body, in, out, state)
    }
  }

  def checkBody(id: Id, body: Body, in: List[Formal], out: List[Formal], state: List[Formal] = Nil): Unit = {
    val inVars: List[Id] = in

    // procedure must at most modify its output variables
    val modifiableVariables: List[Id] = (in ++ out ++ state).distinct
    val modifiedVariables = body.mod
    val illegallyModifiedVariables = modifiedVariables.filter(!modifiableVariables.contains(_))
    ensure(illegallyModifiedVariables.isEmpty, s"The method $id modifies undeclared output parameters ${illegallyModifiedVariables.mkString(", ")}")

    // procedure must at least modify output variables that are not input variables
    val outputsThatMustBeSet = out.map(_.id).filter(!inVars.contains(_))
    val unsetOutputs = outputsThatMustBeSet.filter(!modifiedVariables.contains(_))
    ensure(unsetOutputs.isEmpty, s"The method $id does not modify its output parameters ${unsetOutputs.mkString(", ")}")
  }
}
