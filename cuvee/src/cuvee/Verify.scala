package cuvee

object Verify {
  var debug = false

  def refinement(A: Obj, C: Obj, sim: Sim, st: State, solver: Solver): (List[Expr], Expr) = {
    val (as, cs, defs, phi) = R(A, C, sim, st, solver)
    val (init, ops) = refine(A, as, C, cs, phi)
    val diag = init :: ops
    val (_, conds) = diag.unzip
    (defs, And(conds))
  }

  def R(A: Obj, C: Obj, sim: Sim, st: State, solver: Solver) = sim match {
    case Sim.byFun(fun, recipe) =>
      val as = A.state
      val cs = C.state
      val common = toIds(as).toSet.intersect(toIds(cs).toSet)
      ensure(common.isEmpty, s"state variable names must be disjoint but share: ${common.mkString(", ")}")
      val phi = App(fun, as ++ cs)
      recipe match {
        case Some(recipe) =>
          val synth = Synthesize(A, C, fun, st, solver)
          val defs = synth(recipe)
          (as, cs, defs, phi)
        case None =>
          (as, cs, Nil, phi)
      }
    case Sim.byExpr(as, cs, phi) =>
      (as, cs, Nil, phi)
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
}