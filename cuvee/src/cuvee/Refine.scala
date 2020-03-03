package cuvee

case class Refine(A: Obj, C: Obj, R: Id, state: State, solver: Solver) {
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
   * in are arguments to the formal input parameters
   * out are the formal output parameters of the operation
   * out_ are primed variants for the concrete execution
   */
  def observe(
    op: Id,
    bound: List[Formal],
    as0: List[Expr], cs0: List[Expr],
    in: List[Expr], out: List[Id]) = {
    val out_ = out map (_.prime)
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