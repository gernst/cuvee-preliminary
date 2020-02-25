package cuvee

case class Verify(state: State) {
  import Verify._

  // val simplify = Simplify(state)
  val env = state.env
  val old = Nil
  // import simplify.backend

  /* def verify(phi: Expr, what: String) = {
    val expr = !phi

    if (debug) {
      println(s"(assert ; $what")
      print(Printer.format(expr, "  "))
      println(")")
    }

    val _expr = Eval.eval(expr, env, old, state)
    val __expr = simplify(_expr)

    if (debug) {
      println(s"(assert ; $what (simplified)")
      print(Printer.format(__expr, "  "))
      println(")")
    }

    val (ms, res) = time(backend.check(__expr))
    if (debug) {
      println(res)
    }
    res
  } */

  def apply(spec: Sort, impl: Sort, sim: Sim): Expr = {
    val A = state objects spec
    val C = state objects impl
    val (as, cs, phi) = R(A, C, sim)
    val (init, conds) = refine(A, as, C, cs, phi)
    // println(s"verification conditions for refinement $spec to $impl")
    /* for ((op, phi) <- (init :: conds)) yield {
      verify(phi, s"refine $op")
    } */

    val conj = for ((op, phi) <- (init :: conds))
      yield phi

    And(conj)
  }

  def apply(id: Id) = {
    val proc = state procdefs id
    val phi = contract(proc)
    // verify(phi, s"contract $id")
    phi
  }

  /* def run(solver: Cuvee, report: Report): Unit = {
    for (cmd <- commands) {
      cmd match {
        case DefineProc(id, proc) =>
          report(solver.define(id, proc))
          verifyProcedure(solver, report, proc, None)
        case DefineClass(sort, obj) =>
          report(solver.define(sort, obj))
          obj.ops.foreach(proc => verifyProcedure(solver, report, proc._2, Some(obj)))
        case refinement: DefineRefinement =>
          Verify.verificationConditions(refinement, solver.top).foreach(vc => {
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

  private def verifyProcedure(solver: Cuvee, report: Report, proc: Proc, surroundingClass: Option[Obj]) = {
     report(solver.check(!Verify.verificationCondition(proc, solver.top, surroundingClass)))
  } */
}

object Verify {
  var debug = false

  def R(A: Obj, C: Obj, sim: Sim) = sim match {
    case Sim.byFun(fun) =>
      val as = A.state
      val cs = C.state
      (as, cs, App(fun, as ++ cs))
    case Sim.byExpr(as, cs, phi) =>
      (as, cs, phi)
  }

  def contract(proc: Proc) = {
    val Proc(in, out, pre, post, body) = proc
    Forall(
      in ++ out,
      pre ==> WP(new Block(List(body), true), post))
  }

  def refine(A: Obj, as: List[Formal], C: Obj, cs: List[Formal], R: Expr) = {
    val init = diagram(
      A, as, Id("init") -> A.init,
      C, cs, Id("init") -> C.init,
      True, R)

    val ops = for ((aproc, cproc) <- (A.ops zip C.ops)) yield {
      diagram(
        A, as, aproc,
        C, cs, cproc,
        R, R)
    }

    (init, ops.toList)
  }

  def diagram(
    A: Obj, as: List[Formal], aproc: (Id, Proc),
    C: Obj ,cs: List[Formal], cproc: (Id, Proc),
    R0: Expr, R1: Expr): (Id, Expr) = {

    val (aop, ap) = aproc
    val (cop, cp) = cproc

    ensure(aop == cop, "mismatching operation", aop, cop)

    val Proc(ai, ao, _, _, _) = ap
    val Proc(ci, co, _, _, _) = cp

    val ci_ = ci map (_.prime)
    val co_ = co map (_.prime)

    val (apre, _, abody) = ap.call(A.state, as, ai, ao)
    val (cpre, _, cbody) = cp.call(C.state, cs, ci_, co_)

    val phi = if (aop == Id("init")) {
      (apre && cpre) ==> WP(cbody, Dia(abody, R1))
    } else {
      val in = Eq(ai, ci_)
      val out = Eq(ao, co_)

      in ==>
        ((apre && R0) ==>
          (cpre && WP(cbody, Dia(abody, out && R1))))

    }
    (aop, Forall(
      as ++ ai ++ ao ++ cs ++ ci_ ++ co_,
      phi))
  }

  /**
   * Generates the verification condition for a procedure declaration.
   *
   * @param state auxiliary function/constant definitions
   * @return an all-quantified expression
   */
  def verificationCondition(proc: Proc, state: State, containingClass: Option[Obj]): Expr = {
    val env0 = containingClass match {
      case None => state.env.bind(proc.in)
      case Some(cls) => state.env bind proc.in bind cls.state
    }

    // we evaluate the precondition just to make sure that it is safe, i.e.:
    // - does not contain refer to any "old" values
    // - only refers to input variables and global identifiers
    Eval.eval(proc.pre, env0, List(), state)

    val env1 = env0.bind(proc.out)
    val wpRaw = WP(proc.body, proc.post)
    val wpEval = Eval.eval(wpRaw, env1, List(env0), state)
    Forall((proc.in ++ proc.out ++ containingClass.map(_.state).getOrElse(Nil)).distinct, proc.pre ==> wpEval)
  }
}