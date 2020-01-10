package cuvee

case class Infer(A: Obj, C: Obj, R: Id) {
  def infer(as: List[Pat], cs: List[Pat], ctx: List[Expr]): List[Expr] = {
    ???
  }

  /**
   * Find constraints
   *  - from abstract transitions as0 -> as1 and as1 -> as0
   *  - from the corresponding concrete transitions, where cs0 is given
   *  - specifically, add some R(as1, cs1) for a newly found cs1
   */
  def recurse(as0: List[Pat], cs0: List[Pat], as1: List[Pat], ctx: List[Expr]): List[Expr] = {
    val cs1: List[Id] = ???
    val rec = App(R, as1 ++ cs1)
    ???
  }

  /**
   * Synthesize recursive calls with constraints
   *  @param a0 == as0(pos)
   */
  def recurse(a0: Pat, pos: Int, hyp: List[Int], as0: List[Pat], cs0: List[Pat], ctx: List[Expr]): List[Expr] = a0 match {
    case _: Id =>
      // No recursive invocation
      Nil
    case UnApp(fun, args) =>
      for (i <- hyp) yield {
        // a1 is the argument of the constructor for which a recursive call should be generated
        val a1 = args(i)
        val as1 = as0 updated (pos, a1)
        val phis = recurse(as0, cs0, as1, ctx)
        And(phis)
      }
  }

  /**
   * Introduce a recursive definition by induction
   *  @param sort the name of the data type used for the induction scheme
   *  @param data the constructor definitions
   *  @param pos is the position of the argument used for induction
   *  @param as, cs the list of state variables under consideration
   *  @param ctx the current context
   */
  def induct(sort: Sort, data: Datatype, pos: Int, as: List[Id], cs: List[Id], ctx: List[Expr]): Expr = {
    val Datatype(params, constrs) = data
    ensure(pos < as.length, "unsupported: induction over concrete state", pos, as, cs)
    ensure(params.isEmpty, "unsupported: induction over parametric data types", sort, data)

    val arg = as(pos)

    val cases = for (constr <- constrs) yield {
      induct(sort, constr, pos, as, cs, ctx)
    }

    Match(arg, cases)
  }

  /**
   * A single case for a given constructor
   */
  def induct(sort: Sort, constr: Constr, pos: Int, as: List[Id], cs: List[Id], ctx: List[Expr]): Case = {
    val Constr(id, sels) = constr

    // fresh variables for each constructor argument,
    // named as the selectors
    val args = for (sel <- sels)
      yield Expr.fresh(sel.id)

    // recursive positions of constructor arguments
    // for which an inductive hypothesis in the form of a recursive call is generated
    val hyp = for ((sel, i) <- sels.zipWithIndex if sel.typ == sort)
      yield i

    val pat = UnApps(id :: args)
    val as_ = as updated (pos, pat)

    // synthesize constraints for this case
    val phis = recurse(pat, pos, hyp, as_, cs, ctx)
    val expr = And(phis)

    Case(pat, expr)
  }
}