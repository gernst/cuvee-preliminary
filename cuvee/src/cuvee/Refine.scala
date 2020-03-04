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
    as0: List[Expr], cs0: List[Expr]) = {

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
    as0: List[Expr], cs0: List[Expr]) = {

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
    as0: List[Expr], cs0: List[Expr]) = {

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
      (fresh, And(path), fin, out)
    }

    (pre, _paths)
  }

  type LockstepTest = (List[Formal], Expr, List[Expr], List[Expr], List[Formal], Expr, List[Expr], List[Expr]) => Boolean

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
   * a constraint (that includes equivalence of outputs),
   * and final states for each procedure
   */
  def locksteps(
    aproc: Proc, cproc: Proc,
    as0: List[Expr], cs0: List[Expr],
    in: List[Expr],
    use: LockstepTest) = {

    ensure(aproc.sig == cproc.sig, "incompatible signatures", aproc, cproc)
    val (apre, apaths) = steps(aproc, as, as0, in)
    val (cpre, cpaths) = steps(cproc, cs, cs0, in)

    val _paths = for (
      (aex, apath, as1, aout) <- apaths;
      (cex, cpath, cs1, cout) <- cpaths if use(aex, apath, as1, aout, cex, cpath, cs1, cout)
    ) yield {
      ensure(aex.ids.toSet disjoint cex.ids.toSet, "unsupported", "overlapping auxiliary variables", aex, cex)

      val ex = aex ++ cex
      val path = Eq(aout, cout) && apath && cpath
      (ex, path, as1, cs1)
    }

    val pre = apre && cpre
    (pre, _paths)
  }

  def lemmas = {

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