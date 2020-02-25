package cuvee

case class Verify(state: State) {
  import Verify._

  def apply(spec: Sort, impl: Sort, sim: Sim): Expr = {
    val A = state objects spec
    val C = state objects impl
    val (as, cs, phi) = R(A, C, sim)
    val (init, conds) = refine(A, as, C, cs, phi)

    val conj = for ((op, phi) <- (init :: conds))
      yield phi

    And(conj)
  }

  def apply(id: Id) = {
    val proc = state procdefs id
    val phi = contract(proc)
    phi
  }
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
}