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
    val And(args) = phi
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
      True, Rxs)

    val ops = for ((aproc, cproc) <- (this.ops zip that.ops)) yield {
      diagram(
        as, as1, aproc,
        cs, cs1, cproc,
        Rxs, Rxs1)
    }

    (Rxs, as, cs, init, ops)
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

      val (rxs, as, cs, init, ops) = a refine c

      val _init = Simplify.simplify(init)
      println(_init)

      for (op <- ops) {
        val _op = Simplify.simplify(op)
        println(_op)
      }
    }
  }

}