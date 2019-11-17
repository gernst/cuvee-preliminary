package cuvee

import java.io.File

case class Proc(in: List[Formal], out: List[Formal], pre: Expr, body: Prog) {
  def sig = (in map (_.typ), out map (_.typ))
}

case class Obj(state: List[Formal], init: Proc, ops: List[(String, Proc)]) {
}

object Refine {
  import scala.language.implicitConversions
  implicit def toIds(formals: List[Formal]) = formals map (_.id)

  case class file(afile: File, cfile: File) extends Source {
    val a = Source.file(afile)
    val c = Source.file(cfile)

    def run(solver: Solver, report: Report) {

    }
  }

  def refine(a: Obj, c: Obj) = {
    val as = a.state
    val cs = c.state // map (_.prime)

    val ax = toIds(as)
    val cx = toIds(cs)

    val xs = ax ++ cx
    val R = Id("R")
    val Rxs = App(R, xs)

    val init = diagram(
      a.state, as, ax, "init" -> a.init,
      c.state, cs, cx, "init" -> c.init,
      True, Rxs)

    val ops = for ((aproc, cproc) <- (a.ops zip c.ops)) yield {
      diagram(
        a.state, as, ax, aproc,
        c.state, cs, cx, cproc,
        Rxs, Rxs)
    }

    (Rxs, as, cs, init, ops.toList)
  }

  def diagram(
    as0: List[Formal], as: List[Formal], ax: List[Id], aproc: (String, Proc),
    cs0: List[Formal], cs: List[Formal], cx: List[Id], cproc: (String, Proc),
    R0: Expr, R1: Expr): Expr = {

    val (aop, ap) = aproc
    val (cop, cp) = cproc

    val ai = ap.in
    val ao = ap.out

    val ci = cp.in
    val co = cp.out map (_.prime)

    val pre = Eq(ai, ci)
    val post = Eq(ao, co)

    val (apre, abody) = call(ap, as0, ax, ai, ao)
    val (cpre, cbody) = call(cp, cs0, cx, ci, co)

    Forall(
      as ++ ai ++ ao ++ cs ++ ci ++ co,
      pre ==>
        (apre && R0) ==>
        (cpre && WP(cbody, Dia(abody, post && R1))))
  }

  def call(proc: Proc, ps: List[Formal], xs: List[Id], xi: List[Id], xo: List[Id]): (Expr, Prog) = {
    val Proc(pi, po, pre, body) = proc
    val formals = ps ++ pi ++ po
    val args = xs ++ xi ++ xo

    val re = Expr.subst(formals, args)
    (pre rename re, body rename re)
  }
}