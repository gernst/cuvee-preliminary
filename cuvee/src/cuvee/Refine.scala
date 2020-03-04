package cuvee

case class Refine(A: Obj, C: Obj, R: Id, state: State, solver: Solver) {
  val as = A.state
  val cs = C.state

  val ax: List[Id] = as
  val cx: List[Id] = cs

  /**
   * Synthesize constraints for a recursive definition
   * bound are all variables in scope
   * as0, cs0 are the initial states (left-hand side of definition)
   */
  def induct(
    bound: List[Formal],
    as0: List[Expr], cs0: List[Expr]) = {

  }

  /**
   * Perform induction over a given data type
   * pos is the inductive position in A.state
   */
  def induct(
    bound: List[Formal],
    sort: Sort, dt: Datatype, pos: Int,
    as0: List[Expr], cs0: List[Expr]) = {

    val ai = ax(pos)

    for ((constr, args, hyps) <- dt.induction(ai, sort)) yield {
      val vs: List[Id] = args
      val pat = Apps(constr :: vs)

      val as_args = as patch (pos, args, 1)

      val bound = as_args ++ cs

      val ax0 = ax updated (pos, pat)
      val lhs = App(R, ax0 ++ cx)

      if (hyps.isEmpty) {
        define(bound, lhs,
          base(bound, ax0, cx))
      } else {
        val cases = for (hyp <- hyps) yield {
          val arg = vs(hyp)
          rec(bound, pos, arg, ax0, cx)
        }

        define(
          bound,
          lhs, And(cases))
      }
    }
  }

  /**
   * Synthesize constraints from observing some outputs/preconditions
   * bound are all variables in scope
   * as0, cs0 are the initial states (left-hand side of definition)
   * consider those operations only that can take as0 resp. cs0 as input (i.e. precondition is not falsified)
   */
  def outputs(
    bound: List[Formal],
    as0: List[Expr], cs0: List[Expr]) {

  }

  /**
   * Synthesize the base case where
   * as0, cs0 are the initial states (left-hand side of definition)
   */
  def base(
    bound: List[Formal],
    as0: List[Expr], cs0: List[Expr]): Expr = {
    ???
  }

  /**
   * Synthesise recursive case where
   * bound are all variables in scope
   * pos is the inductive position in A.state
   * arg is the corresponding argument for the recursive call (e.g. xs in (cons x xs))
   * as0, cs0 are the initial states (left-hand side of definition)
   */
  def rec(
    bound: List[Formal],
    pos: Int, arg: Id,
    as0: List[Expr], cs0: List[Expr]): Expr = {
    val phis = consume(bound, pos, arg, as0, cs0)
    Simplify.and(phis.flatten)
  }

  /**
   * Collect constraints from transitions that are guaranteed to produce as0, cs0
   * bound are all variables in scope
   */
  def produce(
    bound: List[Formal],
    pos: Int, arg: Id,
    as0: List[Expr], cs0: List[Expr]) = {

  }

  /**
   * Collect constraints from transitions that are guaranteed to consume as0, cs0
   * bound are all variables in scope
   */
  def consume(
    bound: List[Formal],
    pos: Int, arg: Id,
    as0: List[Expr], cs0: List[Expr]): List[List[Expr]] = {

    val ops = A.ops zip C.ops

    for (((aname, aproc), (cname, cproc)) <- ops) yield {
      ensure(aname == cname, "operations must occur in same order", A.ops, C.ops, aname, cname)
      val in0: List[Id] = aproc.in
      val (apre, cpre, steps) = locksteps(aproc, cproc, as0, cs0, in0)
      val pre = apre && cpre

      for (step <- steps if step.a isConsumer (pos, arg)) yield {
        Simplify.norm(pre && step.constraints)
      }
    }
  }

  /**
   * Collect constraints from lockstep transitions of a given operation
   * op is the name of the operation
   * bound are all variables in scope
   * as0, cs0 are the initial states (left-hand side of definition)
   * in0 are arguments to the formal input parameters
   */
  def observe(
    name: Id,
    bound: List[Formal],
    as0: List[Expr], cs0: List[Expr],
    in0: List[Expr]) = {

    val aproc = A op name
    val cproc = C op name

    val (apre, cpre, steps) = locksteps(aproc, cproc, as0, cs0, in0)
  }

  case class Step(fresh: List[Formal], path: Expr, fin: List[Expr], out: List[Expr]) {
    def bound = Set(fresh map (_.id): _*)

    def ensures(post: Expr) = {
      Forall(fresh, path ==> post)
    }

    def withPost(post: Expr) = {
      Exists(fresh, path && post)
    }

    def isConsumer(pos: Int, arg: Expr) = {
      fin(pos) == arg
    }
  }

  case class Lockstep(a: Step, c: Step) {
    def bound = a.bound ++ c.bound
    def fresh = a.fresh ++ c.fresh
    def path = a.path && c.path

    def ensures(post: Expr) = {
      Exists(fresh, path ==> post)
    }

    def withPost(post: Expr) = {
      Exists(fresh, path && post)
    }

    def outputEqs = {
      Eq(a.out, c.out)
    }

    def constraints = {
      withPost(outputEqs && recursiveCall)
    }

    def recursiveCall = {
      App(R, a.fin ++ c.fin)
    }
  }

  /**
   * Collect constraints from paths through a procedure
   *
   * proc is some procedure
   * xs are the state variables of the respective object
   * init are initial values for these
   * in are the given inputs
   *
   * result is an instantiated precondition and all paths through the procedure
   * each path provides some fresh variables, a path constraint,
   * the final state and the resulting outputs
   */
  def steps(
    proc: Proc,
    xs: List[Formal],
    init: List[Expr],
    in: List[Expr]) = {
    val (pre, paths) = Eval.paths(proc, xs, init, in, state)

    val _paths = for (Path(fresh, path, Env(su, _)) <- paths) yield {
      val fin = xs map (_ subst su)
      val out = proc.out map (_ subst su)
      Step(fresh, And(path), fin, out)
    }

    (pre, _paths)
  }

  /**
   * Collect constraints from lockstep transitions of aproc and cproc
   *
   * as0, cs0 are the initial states, repectively
   * in is the common input
   * use is a predicate that filters out undesired executions,
   * e.g. used to select only consumer/producers transitions
   *
   * result is the combined precondition and all pairwise combinations of paths
   * through the procedures, each providing auxiliary variables,
   * constraints,
   */
  def locksteps(
    aproc: Proc, cproc: Proc,
    as0: List[Expr], cs0: List[Expr],
    in0: List[Expr]) = {

    ensure(aproc.sig == cproc.sig, "incompatible signatures", aproc, cproc)
    val (apre, asteps) = steps(aproc, as, as0, in0)
    val (cpre, csteps) = steps(cproc, cs, cs0, in0)

    val _steps = for (
      astep <- asteps; cstep <- csteps
    ) yield {
      ensure(astep.bound disjoint cstep.bound, "unsupported", "overlapping auxiliary variables", astep, cstep)
      Lockstep(astep, cstep)
    }

    (apre, cpre, _steps)
  }

  def lemmas = {

  }

  /**
   *  Simplify right hand side and build a quantified equation
   */
  def define(bound: List[Formal], lhs: Expr, rhs: Expr): Expr = {
    val simplify = Simplify(solver)
    solver.scoped {
      solver.bind(bound)
      val _rhs = simplify(rhs)
      Forall(bound, lhs === _rhs)
    }
  }

  /**
   * Determine positions of abstract state variables which admit induction over an ADT
   */
  def inductivePositions(xs: List[Formal]) = {
    for ((Formal(_, sort: Sort), i) <- xs.zipWithIndex if state.datatypes contains sort) yield {
      val dt = state datatypes sort
      (sort, dt, i)
    }
  }
}