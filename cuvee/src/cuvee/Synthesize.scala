package cuvee

sealed trait Recipe

object Recipe {
  case object auto extends Recipe
  case object output extends Recipe
}

case class Synthesize(A: Obj, C: Obj, R: Id, state: State, solver: Solver) {
  val as = A.state
  val cs = C.state
  val as_ = as map (_.prime)
  val cs_ = cs map (_.prime)
  val ainit = A.init
  val cinit = C.init

  def inductivePositions = {
    for ((Formal(_, sort: Sort), i) <- as.zipWithIndex if state.datatypes contains sort) yield {
      val dt = state datatypes sort
      (sort, dt, i)
    }
  }

  def apply(recipe: Recipe): List[Expr] = recipe match {
    case Recipe.auto =>
      fromConsumer()
    case Recipe.output =>
      fromOutput()
  }

  ensure(
    A.state.toSet disjoint C.state.toSet,
    "overlapping state variables", A, C)

  def fromOutput() = {
    Nil
  }

  def fromConsumer() = {
    val (sort, dt, pos) = inductivePositions.head
    val exprs = induct(sort, dt, pos)

    for (phi <- exprs)
      println(Printer.format(phi, ""))
    exprs
  }

  def infer(as: List[Pat], cs: List[Pat], ctx: List[Expr]): List[Expr] = {
    ???
  }

  /* def step(proc: Proc, ps: List[Formal], xs: List[Id]): List[Expr] = {
    val (xi, xo, pre, post, prog) = proc call (ps, xs)
    val paths = Eval.rel(prog, ps ++ xi ++ xo, state)
    List(pre, Or(paths map (_.toExpr)))
  } */

  def step(proc: Proc, ps: List[Formal], xs0: List[Expr], xs1: List[Id]) = {
    val (xi, xo, pre, post, prog) = proc call (ps, xs1)
    val su = Expr.subst(xs1, xs0)
    val ty = ps map (_.typ)
    val env0 = Env(su, Map(xs1 zip ty: _*))
    val env1 = env0 bind (xi ++ xo)
    println("  env: " + env1)
    val _pre = Eval.eval(pre, env1, List(), state)
    val paths = Eval.rel(List(prog), env1, List(), state)
    List(_pre, Or(paths map (_.toExpr)))
  }

  def lockstep(
    aproc: Proc, as0: List[Expr], as1: List[Id],
    cproc: Proc, cs0: List[Expr], cs1: List[Id]) = {
    val aphi = step(aproc, as, as0, as1)
    val cphi = step(cproc, cs, cs0, cs1)
    aphi ++ cphi
  }

  /**
   * Find constraints
   *  - from abstract transitions as0 -> as1 and as1 -> as0
   *  - from the corresponding concrete transitions, where cs0 is given
   *  - specifically, add some R(as1, cs1) for a newly found cs1
   */
  def recurse(as0: List[Expr], cs0: List[Id], as1: List[Id], ctx: List[Expr]): List[Expr] = {
    val cs1 = cs0 map (_.prime)
    val rec = App(R, as1 ++ cs1)

    val _as1 = (as zip as1) map { case (a, a1) => Formal(a1, a.typ) }
    val _cs1 = (cs zip cs1) map { case (c, c1) => Formal(c1, c.typ) }

    val ops = for (((aname, aproc), (cname, cproc)) <- A.ops zip C.ops) yield {
      val phis = lockstep(aproc, as0, as1, cproc, cs0, cs1)
      assert(aname == cname)
      println(aname)
      println("  A: " + sexpr(as0) + " ~> " + sexpr(as1))
      println("  C: " + sexpr(cs0) + " ~> " + sexpr(cs1))
      for (phi <- phis if phi != True)
        println("  " + phi)
      println()
      Exists(_as1 ++ _cs1, And(phis))
    }
    List(rec, Or(ops))
  }

  def base(fun: Id, pos: Int, as0: List[Id], cs0: List[Id], ctx: List[Expr]): List[Expr] = {
    lockstep(ainit, as, as0, cinit, cs, cs0)
    // step(cinit, cs, cs0)
  }

  def recurse(fun: Id, args: List[Id], pos: Int, hyp: List[Int], as0: List[Id], cs0: List[Id], ctx: List[Expr]): List[Expr] = {
    for (i <- hyp) yield {
      // a0 is the current pattern for the inductive argument
      val a0 = App(fun, args)
      // a1 is the argument of the constructor for which a recursive call should be generated
      val a1 = args(i)
      val as0_ = as0 updated (pos, a0)
      val as1 = as0 updated (pos, a1)
      val phis = recurse(as0_, cs0, as1, ctx)
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
  def induct(sort: Sort, data: Datatype, pos: Int, as: List[Formal], cs: List[Formal], ctx: List[Expr]): List[Expr] = {
    val Datatype(params, constrs) = data
    ensure(pos < as.length, "unsupported: induction over concrete state", pos, as, cs)
    ensure(params.isEmpty, "unsupported: induction over parametric data types", sort, data)

    for (constr <- constrs) yield {
      val (id, args, rhs) = induct(sort, constr, pos, as, cs, ctx)
      val pat = Apps(id :: (args: List[Id]))
      val ax: List[Id] = as
      val cx: List[Id] = cs
      val ax_ = ax updated (pos, pat)
      val as__ = as patch (pos, args, 1)
      Forall(
        as__ ++ cs,
        App(R, ax_ ++ cx) === rhs)
    }
  }

  def induct(sort: Sort, data: Datatype, pos: Int): List[Expr] = {
    induct(sort, data, pos, as, cs, Nil)
  }

  /**
   * A single case for a given constructor
   */
  def induct(sort: Sort, constr: Constr, pos: Int, as: List[Formal], cs: List[Formal], ctx: List[Expr]): (Expr, List[Formal], Expr) = {
    val Constr(id, sels) = constr

    // fresh variables for each constructor argument,
    // named as the selectors
    val args = for (Sel(id, typ) <- sels)
      yield Formal(Expr.fresh(id), typ)

    // recursive positions of constructor arguments
    // for which an inductive hypothesis in the form of a recursive call is generated
    val hyp = for ((sel, i) <- sels.zipWithIndex if sel.typ == sort)
      yield i

    // synthesize constraints for this case
    val phis = if (args.isEmpty) {
      base(id, pos, as, cs, ctx)
    } else {
      recurse(id, args, pos, hyp, as, cs, ctx)
    }

    (id, args, And(phis))
  }
}