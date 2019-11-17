package cuvee

import java.io.File

case class Step(pre: List[Expr], eqs: List[Eq]) {

}

object Step {
  def apply(phi: Expr): Step = {
    val And(args) = phi
    val pre = args.filter { !_.isInstanceOf[Eq] }
    val eqs = args collect { case eq: Eq => eq }
    Step(pre, eqs)
  }
}

case class Trans(init: Step, ops: List[Step]) {

}

object Trans {
  def apply(cmds: List[Cmd]): Trans = {
    val exprs = cmds collect {
      case Assert(expr) => expr
    }
    val init :: ops = exprs
    Trans(Step(init), ops map (Step(_)))
  }
}

object Refine {
  import scala.language.implicitConversions

  case class file(afile: File, cfile: File) extends Source {
    val asrc = Source.file(afile)
    val csrc = Source.file(cfile)

    def run(solver: Solver, report: Report) {
      val acmds = asrc.cmds
      val ccmds = csrc.cmds
      val a = Trans(acmds)
      val c = Trans(ccmds)
      println(a)
      println(c)
    }
  }

}