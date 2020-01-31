package cuvee

object Synthesize {
  def observe(a: Trans, c: Trans, st: State) = {
    val as = a.state
    val as1 = a.state1
    val cs = c.state
    val cs1 = c.state1

    val init = io(
      as, as1, a.init,
      cs, cs1, c.init,
      st)

    val ops = for ((aproc, cproc) <- (a.ops zip c.ops)) yield {
      io(
        as, as1, aproc,
        cs, cs1, cproc,
        st)
    }

    val EQ = Id("eq")

    (EQ, as, cs, And(init :: ops))
  }

  def locksteps(a: Trans, c: Trans, st: State) = {
    val as = a.state
    val as1 = a.state1
    val cs = c.state
    val cs1 = c.state1

    val init = lockstep(
      as, as1, a.init,
      cs, cs1, c.init,
      st)

    val ops = for ((aproc, cproc) <- (a.ops zip c.ops)) yield {
      lockstep(
        as, as1, aproc,
        cs, cs1, cproc,
        st)
    }

    (as, as1, cs, cs1, init, Or(ops))
  }

  def io(
    as: List[Formal], as1: List[Formal], astep: Step,
    cs: List[Formal], cs1: List[Formal], cstep: Step,
    st: State) = {
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
    val out_ = out map (_.prime)

    val ts = out map st.const
    val st_ = (out_ zip ts).foldLeft(st) {
      case (st, (id, Formal(_, typ))) =>
        st declare (id, Nil, typ)
    }

    val re = Expr.subst(out, out_)
    val cpre_ = cpre rename re
    val ceq_ = ceq rename re

    val post = Eq(out, out_)

    val xs = (in ++ out ++ out_) map st_.const

    Forall(
      xs,
      Exists(as1 ++ cs1, apre && cpre_ && aeq && ceq_)
        ==> post)
  }

  def lockstep(
    as: List[Formal], as1: List[Formal], astep: Step,
    cs: List[Formal], cs1: List[Formal], cstep: Step,
    st: State) = {
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
    val xs = (in ++ out) map st.const

    Exists(
      xs,
      apre && cpre && aeq && ceq)
  }
}