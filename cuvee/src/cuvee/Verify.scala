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
      pre ==> WP(body, post))
  }

  def refine(A: Obj, as: List[Formal], C: Obj, cs: List[Formal], R: Expr) = {
    val init = diagram(
      as, Id("init") -> A.init,
      cs, Id("init") -> C.init,
      True, R)

    val ops = for ((aproc, cproc) <- (A.ops zip C.ops)) yield {
      diagram(
        as, aproc,
        cs, cproc,
        R, R)
    }

    (init, ops.toList)
  }

  def diagram(
    as: List[Formal], aproc: (Id, Proc),
    cs: List[Formal], cproc: (Id, Proc),
    R0: Expr, R1: Expr): (Id, Expr) = {

    val (aop, ap) = aproc
    val (cop, cp) = cproc

    ensure(aop == cop, "mismatching operation", aop, cop)

    val Proc(ai, ao, _, _, _) = ap
    val Proc(ci, co, _, _, _) = cp

    val co_ = co map (_.prime)

    val in = Eq(ai, ci)
    val out = Eq(ao, co_)

    val (apre, apost, abody) = ap.call(as, as, ai, ao)
    val (cpre, cpost, cbody) = cp.call(as, as, ci, co_)

    val phi = Forall(
      as ++ ai ++ ao ++ cs ++ ci ++ co_,
      in ==>
        (apre && R0) ==>
        (cpre && WP(cbody, Dia(abody, out && R1))))

    (aop, phi)
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

  def verificationConditions(refinement: DefineRefinement, st: State): List[Expr] = {

    def prefixedMembers(obj: Obj, instanceName: Id): List[Formal] = obj.state.map(f => Formal(Id(s"${instanceName}_${f.id}"), f.typ))

    def prefixingMembers(obj: Obj, instanceName: Id): Map[Id, Id] = obj.state.map(f => (f.id -> Id(s"${instanceName}_${f.id}"))) toMap

    def proc(obj: Obj, id: Id): Proc = obj.ops.filter(_._1 == id).head._2

    val DefineRefinement(abstr, concr, relation) = refinement
    val Formal(aid, asort: Sort) = abstr
    val Formal(cid, csort: Sort) = concr

    val ac = st objects asort
    val cc = st objects csort
    val commonProcs = ac.ops map (_._1) intersect cc.ops.map(_._1)

    val aPrefixed = prefixedMembers(ac, abstr.id)
    val cPrefixed = prefixedMembers(cc, concr.id)

    val aPrefixing = prefixingMembers(ac, abstr.id)
    val cPrefixing = prefixingMembers(cc, concr.id)

    val init: Expr = {
      val ap = ac.init
      if (ap.out.nonEmpty) {
        throw Error(s"init method of ${abstr.typ} must not declare output parameters")
      }
      val cp = cc.init
      if (cp.out.nonEmpty) {
        throw Error(s"init method of ${concr.typ} must not declare output parameters")
      }

      val aPre = ap.pre rename aPrefixing
      val cPre = cp.pre rename cPrefixing rename cp.in.priming

      val aBody = ap.body replace aPrefixing
      val cBodyPrime = cp.body replace cPrefixing replace cp.in.distinct.priming

      val allVars = aPrefixed ++ cPrefixed ++ ap.in ++ cp.in.prime
      val env_ = st.env bind allVars
      val unqualified = (aPre && cPre) ==> Eval.eval(WP(Block(List(aBody, cBodyPrime)), relation), env_, List(), st)

      Forall(allVars, unqualified)
    }

    val nonInitProcs: List[Expr] = commonProcs filter (_.name != "init") map (name => {
      val ap = proc(ac, name)
      val cp = proc(cc, name)
      if (ap.in != cp.in || ap.out != cp.out) {
        throw Error(s"Signatures of ${abstr.typ}.$name and ${concr.typ}.$name do not match")
      }

      val insEq = And(ap.in.map(f => f.id === f.id.prime))
      val outsEq = And(ap.out.map(f => f.id === f.id.prime))

      val aPre = ap.pre rename aPrefixing // prefix class members with name of class instance
      val cPre = cp.pre rename cPrefixing // don't prime variables for precondition

      val aBody = ap.body replace aPrefixing
      val cBodyPrime = cp.body replace cPrefixing replace (cp.in ++ cp.out).distinct.priming // in the body of C prime all vars for sequential execution

      val allVars = aPrefixed ++ cPrefixed ++ ap.in ++ ap.out ++ cp.in.prime ++ cp.out.prime
      val env_ = st.env bind allVars
      val unqualified = relation ==> (aPre ==> (cPre && (insEq ==> Eval.eval(WP(Block(List(aBody, cBodyPrime)), outsEq && relation), env_, List(), st))))

      Forall(allVars, unqualified)
    })

    init :: nonInitProcs
  }
}