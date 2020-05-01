package cuvee

case class Prove(backend: Solver) {

  import Simplify._

  def apply(phi: Expr, st: State): Expr = {
    prove(phi, st)
  }

  def prove(todo: List[Expr], neg: Boolean, st: State): List[Expr] = todo match {
    case Nil =>
      Nil
    case phi :: rest =>
      val _phi = prove(phi, st)
      val __phi = if (neg) !_phi else _phi
      val _rest = backend.asserting(__phi) {
        prove(rest, neg, st)
      }
      _phi :: _rest
  }

  def prove(phi: Expr, st: State): Expr = phi match {
    case t@(True | False) => t

    case Imp(phi, psi) =>
      val _psi = backend.asserting(phi) {
        prove(psi, st)
      }
      imp(phi, _psi)

    case And(args) =>
      val _args = prove(args, neg = false, st)
      and(_args)

    case f@Forall(bound, _) =>
      // we only want to get fresh names for unused variable names or this turns unreadable
      val fresh = Expr.fresh(bound.ids.toSet & st.constants.ids.toSet)
      val Forall(bound_, body_) = f.rename(fresh, fresh)
      val _body = backend.binding(bound_) {
        prove(body_, st.declareConstants(bound_))
      }
      forall(bound_, _body)

    case Eq(left, right)
      // since we never have local variables, we do a type check based on globals
      if Check(st).infer(left, Map.empty, None) == Sort.bool
        && Check(st).infer(right, Map.empty, None) == Sort.bool =>
      prove(And(left ==> right, right ==> left), st)

    case Ite(test, left, right) =>
      prove(And(test ==> left, !test ==> right), st)

    case App(Id("iff", None), List(left, right)) =>
      prove(And(left ==> right, right ==> left), st)

    case App(r, args) if (st.fundefs contains r)
      // do not substitute recursive functions
      && !(evaluations(st.fundefs(r)._2) contains r) =>
      val (formals, body) = st.fundefs(r)
      prove(body.subst(Expr.subst(formals, args)), st)

    case _ =>
      val cs = cases(phi)
      qualify(cs.distinct, phi, st)
  }

  def qualify(cs: List[Expr], phi: Expr, st: State): Expr = cs match {
    case _ if backend isTrue phi =>
      True
    case Nil =>
      phi
    case pre :: rest =>
      println("qualifying: " + pre + "==> " + phi)
      val phi1 = backend.asserting(pre) {
        imp(pre, qualify(rest, phi, st))
      }
      val phi2 = backend.asserting(!pre) {
        imp(not(pre), qualify(rest, phi, st))
      }
      and(List(phi1, phi2))
  }

  /**
   * Finds the ids of all evaluated functions
   */
  def evaluations(expr: Expr): Set[Id] = expr match {
    // function evaluation
    case App(fun, args) => Set(fun) ++ args.toSet.flatMap(evaluations)

    // no function evaluation
    case Id(_, _) | Num(_) | Note(_, _) | As(_, _) => Set.empty

    // collect from parts
    case Eq(left, right) => evaluations(left) ++ evaluations(right)
    case Distinct(exprs) => exprs.toSet.flatMap(evaluations)
    case Ite(test, left, right) => Set(test, left, right).flatMap(evaluations)
    case Let(pairs, body) => evaluations(body) ++ pairs.map(_.e).toSet.flatMap(evaluations)
    case Match(expr, cases) => evaluations(expr) ++ cases.map(_.expr).toSet.flatMap(evaluations)
    case Select(array, index) => evaluations(array) ++ evaluations(index)
    case Store(array, index, value) => Set(array, index, value).flatMap(evaluations)
    case Old(expr) => evaluations(expr)
    case Bind(_, _, body) => evaluations(body)

    // we'll assume for now that these don't turn up in function definitions:
    case Post(_, _, _) => ??? // we're not gonna get into programs
    case Refines(_, _, _) => ??? // we'd have to evaluate this to know
  }

  def cases(expr: Expr): List[Expr] = expr match {
    case Ite(test, left, right) =>
      List(test) ++ cases(left) ++ cases(right)
    case Select(Store(array, index, value), index_) =>
      List(index === index_) ++ cases(array)
    case Eq(left, right) => cases(left) ++ cases(right)
    case App(fun, args) => args flatMap cases
    case _ => List()
  }
}