package cuvee

case class Verify(commands: Iterable[Cmd]) extends Source {
  def run(solver: Solver, report: Report): Unit = solver match {
    case cuvee: Cuvee => run(cuvee, report)
    case _ => ???
  }

  def run(solver: Cuvee, report: Report): Unit = {
    for (cmd <- commands) {
      cmd match {
        case proc: DefineProc =>
          report.apply(solver.push())
          report.apply(solver.assert(!Verify.verificationCondition(proc, solver.top)))
          report.apply(solver.check())
          report.apply(solver.pop())
        case other =>
          solver.exec(other) match {
            case Some(res) => report.apply(res)
            case None => Unit
          }
      }
    }
  }
}

object Verify {
  /**
   * Generates the verification condition for a procedure declaration.
   *
   * @param state auxiliary function/constant definitions
   * @return an all-quantified expression
   */
  def verificationCondition(proc: DefineProc, state: State): Expr = {
    val env0 = state.env.bind(proc.in)

    // we evaluate the precondition just to make sure that it is safe, i.e.:
    // - does not contain refer to any "old" values
    // - only refers to input variables and global identifiers
    Eval.eval(proc.pre, env0, List(), state)

    val env1 = env0.bind(proc.out) // XXX havoc?
    val wpRaw = WP(proc.body, proc.post)
    val wpEval = Eval.eval(wpRaw, env1, List(env0), state)
    Forall((proc.in ++ proc.out).distinct, proc.pre ==> wpEval)
  }
}