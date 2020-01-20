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
          report(solver.define(proc.id, proc.in, proc.out, proc.body, proc.pre, proc.post))
          verifyProcedure(solver, report, proc, None)
        case clazz: DefineClass =>
          report(solver.define(clazz))
          clazz.procs.foreach(verifyProcedure(solver, report, _, Some(clazz)))
        case refinement: DefineRefinement =>
          refinement.verificationConditions(solver.top).foreach(vc => {
            report(solver.check(!vc))
          })
        case other =>
          solver.exec(other) match {
            case Some(res) => report(res)
            case None => Unit
          }
      }
    }
  }

  private def verifyProcedure(solver: Cuvee, report: Report, proc: DefineProc, surroundingClass: Option[DefineClass]) = {
    report(solver.check(!Verify.verificationCondition(proc, solver.top, surroundingClass)))
  }
}

object Verify {
  /**
   * Generates the verification condition for a procedure declaration.
   *
   * @param state auxiliary function/constant definitions
   * @return an all-quantified expression
   */
  def verificationCondition(proc: DefineProc, state: State, containingClass: Option[DefineClass]): Expr = {
    val env0 = containingClass match {
      case None => state.env.bind(proc.in)
      case Some(cls) => state.env bind proc.in bind cls.fields
    }

    // we evaluate the precondition just to make sure that it is safe, i.e.:
    // - does not contain refer to any "old" values
    // - only refers to input variables and global identifiers
    Eval.eval(proc.pre, env0, List(), state)

    val env1 = env0.bind(proc.out)
    val wpRaw = WP(proc.body, proc.post)
    val wpEval = Eval.eval(wpRaw, env1, List(env0), state)
    Forall((proc.in ++ proc.out ++ containingClass.map(_.fields).getOrElse(Nil)).distinct, proc.pre ==> wpEval)
  }
}