package cuvee

case class Verify(commands: Iterable[Cmd]) extends Source {
  def run(solver: Solver, report: Report): Unit = solver match {
    case cuvee: Cuvee => run(cuvee, report)
    case _ => ???
  }

  def run(solver: Cuvee, report: Report): Unit = {
    for (cmd <- commands) {
      cmd match {
        case DefineProc(id, proc) =>
          report(solver.define(id, proc))
          verifyProcedure(solver, report, proc, None)
        case DefineClass(sort, obj) =>
          report(solver.define(sort, obj))
          ???
          // clazz.procs.foreach(verifyProcedure(solver, report, _, Some(clazz)))
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
  }
}

object Verify {
  /**
   * Does some basic checks on a procedure w.r.t. well-definedness. This exludes anything that requires knowledge about
   * the state.
   *
   * @param id    name of the procedure for error messages
   * @param state (optional) state of the surrounding object if this procedure is defined on an object
   */
  def checkProc(id: Id, proc: Proc, state: List[Formal] = List()): Unit = {
    val Proc(in, out, pre, post, body) = proc
    val inVars: List[Id] = in
    val duplicateInputDeclarations = inVars.groupBy(identity).filter(_._2.size > 1)
    if (duplicateInputDeclarations.nonEmpty) {
      throw Error(s"The method $id declares duplicate input parameters ${duplicateInputDeclarations.keys.mkString(", ")}")
    }

    // the outputs may have the same variable name in multiple places if the type is equal.
    // outputs may overlap with inputs but, again, the type must be equal
    val nonUniqueAgruments = (in ++ out).groupBy(_.id).filter(_._2.map(_.typ).distinct.size > 1)
    if (nonUniqueAgruments.nonEmpty) {
      throw Error(s"The method $id declares non-unique type for argument ${nonUniqueAgruments.keys.mkString(", ")}")
    }

    // procedure must at most modify its output variables
    val modifiableVariables: List[Id] = (in ++ out ++ state).distinct
    val modifiedVariables = body.mod
    val illegallyModifiedVariables = modifiedVariables.filter(!modifiableVariables.contains(_))
    if (illegallyModifiedVariables.nonEmpty) {
      throw Error(s"The method $id modifies undeclared output parameters ${illegallyModifiedVariables.mkString(", ")}")
    }

    // procedure must at least modify output variables that are not input variables
    val outputsThatMustBeSet = out.map(_.id).filter(!inVars.contains(_))
    val unsetOutputs = outputsThatMustBeSet.filter(!modifiedVariables.contains(_))
    if (unsetOutputs.nonEmpty) {
      throw Error(s"The method $id does not modify its output parameters ${unsetOutputs.mkString(", ")}")
    }
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