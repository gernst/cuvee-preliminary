package cuvee

sealed trait Recipe

object Recipe {
  case object auto extends Recipe
  case object output extends Recipe
}

case class Step(id: Id, pre: Expr, xi: List[Formal], xs0: List[Formal], xs1: List[Formal], xo: List[Formal], path: Path) {
  def ensuresPost(that: Expr): Expr = {
    val Path(zs, phis, env) = path
    Forall(
      xi ++ xs0 ++ zs,
      And(pre :: phis) ==> (that subst env.su))
  }

  def withPost(post: Expr): Expr = {
    val Path(zs, phis, env) = path
    Exists(
      xi ++ xs0 ++ zs,
      And(pre :: phis) && (post subst env.su))
  }
}

case class Synthesize(A: Obj, C: Obj, R: Id, state: State, solver: Solver) {
  val as = A.state
  val cs = C.state
  val as_ = as map (_.prime)
  val cs_ = cs map (_.prime)
  val ainit = A.init
  val cinit = C.init

  ensure(
    A.state.toSet disjoint C.state.toSet,
    "overlapping state variables", A, C)

  def apply(recipe: Recipe): List[Expr] = recipe match {
    case Recipe.auto =>
      val inds = inductivePositions(as)
      val List((sort, dt, pos)) = inds
      fromConsumer(sort, dt, pos)

    case Recipe.output =>
      fromOutput()
  }

  def fromOutput() = {
    Nil
  }

  def fromConsumer(sort: Sort, dt: Datatype, pos: Int) = {
    val asteps = transitions(A)
    val ax: List[Id] = as
    val cx: List[Id] = cs
    val cx_ : List[Id] = cs_

    val ai = ax(pos)

    for ((constr, args, hyps) <- dt.induction(ai, sort)) yield {
      val vs: List[Id] = args
      val pat = Apps(constr :: vs)

      val as0 = as patch (pos, args, 1)
      val ax0 = ax updated (pos, pat)
      val lhs = App(R, ax0 ++ cx)
      val rhs = False

      Forall(
        as0 ++ cs,
        lhs === rhs)
    }
  }

  /**
   * Determine positions of abstract state variables
   * which admit induction over an ADT.
   */
  def inductivePositions(xs: List[Formal]) = {
    for ((Formal(_, sort: Sort), i) <- xs.zipWithIndex if state.datatypes contains sort) yield {
      val dt = state datatypes sort
      (sort, dt, i)
    }
  }

  /**
   * Compute all possible transitions of operations of an object.
   * Return steps of operation name, precondition, pairs of state variables, and path.
   */
  def transitions(o: Obj) = {
    val ps0 = o.state
    val ps1 = ps0 map (_.prime)
    for (
      (id, proc) <- o.ops;
      (_pre, xi, xo, _paths) = paths(proc, ps0, ps0, ps1);
      _path <- _paths
    ) yield {
      Step(id, _pre, xi, ps0, ps1, xo, _path)
    }
  }

  /**
   * Return a list of assertions that capture conditions
   * under which x occurs as a constructor argument to expression c.
   */
  def isSubterm(smaller: Expr, larger: Expr, typ: Type, dt: Datatype) = {
    for (
      Constr(_, sels) <- dt.constrs;
      Sel(sel, `typ`) <- sels
    ) yield {
      smaller === App(sel, List(larger)) // TODO: lacks test for this constructor
    }
  }

  /**
   * Check if a path is a consumer, i.e., is guarantee to return subterms where pos is the inductive position.
   */
  def isConsumer(step: Step, typ: Type, dt: Datatype, pos: Int) = {
    val x0 = step.xs0(pos)
    val x1 = step.xs1(pos)
    val conds = isSubterm(x1.id, x0.id, typ, dt)
    val phi = step ensuresPost Or(conds)
    solver isTrue phi
  }

  /**
   * Check if a path is a producer, i.e., is guarantee to return constructor terms where pos is the inductive position.
   */
  def isProducer(step: Step, typ: Type, dt: Datatype, pos: Int) = {
    val x0 = step.xs0(pos)
    val x1 = step.xs1(pos)
    val conds = isSubterm(x0.id, x1.id, typ, dt)
    val phi = step ensuresPost Or(conds)
    solver isTrue phi
  }

  def infer(as: List[Pat], cs: List[Pat], ctx: List[Expr]): List[Expr] = {
    ???
  }

  /* def step(proc: Proc, ps: List[Formal], xs: List[Id]): List[Expr] = {
    val (xi, xo, pre, post, prog) = proc call (ps, xs)
    val paths = Eval.rel(prog, ps ++ xi ++ xo, state)
    List(pre, Or(paths map (_.toExpr)))
  } */

  /**
   * Determine all paths through proc (splitting conditionals)
   *  wrt. state parameters ps, such that the path takes a transition from xs0 to xs1.
   *  Return the instantiated precondition as well as the paths,
   *  which store constraints and variable assignments for xs1 in the successor states.
   */
  def paths(proc: Proc, ps: List[Formal], xs0: List[Expr], xs1: List[Id]) = {
    val (xi, xo, pre, post, prog) = proc call (ps, xs1)
    val su = Expr.subst(xs1, xs0)
    val ty = ps map (_.typ)
    val env0 = Env(su, Map(xs1 zip ty: _*))
    val env1 = env0 bind (xi ++ xo)
    val _pre = Eval.eval(pre, env1, List(), state)
    val paths = Eval.rel(List(prog), env1, List(), state)
    (_pre, xi, xo, paths)
  }

  def step(proc: Proc, ps: List[Formal], xs0: List[Expr], xs1: List[Id]) = {
    val (_pre, xi, xo, _paths) = paths(proc, ps, xs0, xs1)
    List(_pre, Or(_paths map (_.toExpr)))
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