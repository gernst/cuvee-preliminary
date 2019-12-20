package cuvee

import java.io.File

case class Step(pre: List[Expr], eqs: List[Eq]) {
  val free = Set((pre ++ eqs) flatMap (_.free): _*)

  val primed = free collect {
    case Id(name, index) if name endsWith "1" =>
      Id(name dropRight 1, index)
  }

  val in = free filter (_.name startsWith "in")
  val out = free filter (_.name startsWith "out")
}

object Step {
  def apply(phi: Expr): Step = {
    val args = And.flatten(phi)
    val pre = args.filter { !_.isInstanceOf[Eq] }
    val eqs = args collect { case eq: Eq => eq }
    Step(pre, eqs)
  }
}

case class Trans(state: List[Formal], init: Step, ops: List[Step]) {
  def state1 = state map {
    case Formal(Id(name, index), typ) =>
      Formal(Id(name + "1", index), typ)
  }

  def refine(that: Trans) = {
    import Trans.diagram

    val as = this.state
    val as1 = this.state1
    val cs = that.state // map (_.prime)
    val cs1 = that.state1

    val R = Id("R")
    val Rxs = App(R, as ++ cs)
    val Rxs1 = App(R, as1 ++ cs1)

    val init = diagram(
      as, as1, this.init,
      cs, cs1, that.init,
      True, Rxs1)

    val ops = for ((aproc, cproc) <- (this.ops zip that.ops)) yield {
      diagram(
        as, as1, aproc,
        cs, cs1, cproc,
        Rxs, Rxs1)
    }

    (R, as, cs, init, ops)
  }
}

object Trans {
  def apply(cmds: List[Cmd], st: State): Trans = {
    val exprs = cmds collect {
      case Assert(expr) => expr
    }
    val steps = exprs map (Step(_))
    val state = steps flatMap (_.primed) map {
      id =>
        val (Nil, typ) = st.funs(id)
        Formal(id, typ)
    }
    val init :: ops = steps
    Trans(state.distinct, init, ops)
  }

  def diagram(
    as: List[Formal], as1: List[Formal], astep: Step,
    cs: List[Formal], cs1: List[Formal], cstep: Step,
    R0: Expr, R1: Expr): Expr = {

    val Step(apres, abody) = astep
    val Step(cpres, cbody) = cstep

    val apre = And(apres)
    val cpre = And(cpres)
    val aeq = And(abody)
    val ceq = And(cbody)

    val ai = astep.in
    val ao = astep.out

    val ci = cstep.in
    val co = cstep.out

    val in = (ai & ci).toList
    val out = (ao & co).toList

    (apre && ceq && R0) ==>
      (cpre && Exists(as1, aeq && R1))
    //    (apre && aeq && ceq && R0) ==>
    //      (cpre && R1)
  }
}

object Refine {
  import scala.language.implicitConversions

  case class file(afile: File, cfile: File) extends Source {
    val asrc = Source.file(afile)
    val csrc = Source.file(cfile)

    def run(solver: Solver, report: Report) = solver match {
      case cuvee: Cuvee => run(cuvee, report)
      case _ => ???
    }

    def run(solver: Cuvee, report: Report) {
      val acmds = asrc.cmds
      val ccmds = csrc.cmds

      val cmds = (acmds ++ ccmds).distinct filterNot (_.isInstanceOf[Assert])

      for (cmd <- cmds) {
        Source.safe(cmd, solver, report)
      }

      val a = Trans(acmds, solver.top)
      val c = Trans(ccmds, solver.top)

      val (_R, as, cs, init, ops) = a refine c
      solver.declare(_R, as ++ cs, Sort.bool)

      val (_EQ, _, _, observe) = Synthesize.observe(a, c, solver.top)
      val xs = as ++ cs
      solver.declare(_EQ, xs, Sort.bool)

      val List(rhs) = ??? // Simplify.con(List(observe), Map(), false)
      solver.assert(Forall(xs, App(_EQ, xs) === rhs))

      val (_, as1, _, cs1, isinit, isop) = Synthesize.locksteps(a, c, solver.top)
      val xs1 = as1 ++ cs1

      val List(ax) = as map (_.id)
      val List(n, ar) = cs map (_.id)
      val List(ax1) = as1 map (_.id)
      val List(n1, ar1) = cs1 map (_.id)

      val base = Forall(
        xs1,
        (ax1 === Id.nil) ==> (App(_R, List(ax1, n1, ar1)) === (isinit && App(_EQ, List(ax1, n1, ar1)))))

      val x = Id("x")
      val rec = Forall(
        xs ++ xs1,
        Exists(List(Formal(x, Sort("Elem"))), ax1 === App(Id.cons, List(x, ax)))
          ==> (App(_R, List(ax1, n1, ar1)) === (isop && App(_EQ, List(ax1, n1, ar1)) && App(_R, List(ax, n, ar)))))

      println(base)
      println(rec)
      solver.assert(base)
      solver.assert(rec)

      report(solver.check(!init))
      for (op <- ops) {
        report(solver.check(!op))
      }
    }
  }

}