package cuvee

case class Simplify(backend: Solver) {
  case object Unsat extends Exception

  def simplify(exprs: List[Expr], pos: Boolean = true): List[Expr] = {
    simplify(exprs, eqs = Map(), pos)
  }

  def simplify(exprs: List[Expr], eqs: Map[Id, Expr], pos: Boolean): List[Expr] = exprs match {
    case Nil =>
      Nil

    case (bind: Bind) :: rest =>
      val _phi = bind // don't simplify axioms
      val _rest = assuming(_phi, eqs, simplify(rest, _, pos))
      _phi :: _rest

    case phi :: rest =>
      val _phi = simplify(phi, eqs, pos)
      val _rest = assuming(_phi, eqs, simplify(rest, _, pos))
      _phi :: _rest
  }

  def apply(phi: Expr): Expr = {
    simplify(phi, eqs = Map(), pos = true)
  }

  def simplify(phi: Expr, eqs: Map[Id, Expr], pos: Boolean): Expr = {
    val _phi = _simplify(phi, eqs, pos)
    //    println("simplify: ")
    //    println("     " + phi)
    //    println("  ~> " + _phi)
    //    println()
    _phi
  }

  def con(exprs: List[Expr], eqs: Map[Id, Expr], pos: Boolean): List[Expr] = exprs match {
    case Nil =>
      Nil

    case phi :: rest =>
      val _phi = simplify(phi, eqs, pos)
      val _rest = assuming(_phi, eqs, con(rest, _, pos))
      _phi :: _rest
  }

  def dis(exprs: List[Expr], eqs: Map[Id, Expr], pos: Boolean): List[Expr] = exprs match {
    case Nil =>
      Nil

    case phi :: rest =>
      val _phi = simplify(phi, eqs, pos)
      val _rest = assuming(!_phi, eqs, dis(rest, _, pos))
      _phi :: _rest
  }

  def _simplify(phi: Expr, eqs: Map[Id, Expr], pos: Boolean): Expr = phi match {
    case _ if isFalse(phi) =>
      False
    case _ if isTrue(phi) =>
      True
    case Not(phi) =>
      val _phi = simplify(phi, eqs, !pos)
      not(_phi)
    case And.nary(args) =>
      val _args = con(args, eqs, pos)
      and(_args)
    case Or.nary(args) =>
      val _args = dis(args, eqs, pos)
      or(_args)
    case Imp(phi, psi) =>
      val _phi = simplify(phi, eqs, !pos)
      val _psi = assuming(_phi, eqs, simplify(psi, _, pos))
      imp(_phi, _psi)
    // HACK
    case Distinct(List(xs, Id.nil)) =>
      val x = Id("x")
      val xs0 = Id("xs0")
      val xs_ = Expr.fresh(xs0)
      Exists(List(Formal(x, Sort("Elem")), Formal(xs_, Sort("Lst"))), xs === x :: xs_)
    /* case Forall(formals, And.nary(args)) =>
      val _args = args map (Forall(formals, _))
      simplify(And(_args), eqs, pos)
    case bind @ Forall(formals, Or.nary(args)) =>
      val bound = bind.bound
      val (move, keep) = args.partition(_.free disjoint bound)
      val bind_ = Forall(formals, Or.nary(keep))
      val bind__ = __simplify(bind_, eqs, pos)
      val rest = assuming(!bind__, eqs, dis(move, _, pos))
      Or(bind__ :: rest)
    case Exists(formals, Or.nary(args)) =>
      val _args = args map (Exists(formals, _))
      simplify(Or(_args), eqs, pos)
    case bind @ Exists(formals, And.nary(args)) =>
      val bound = bind.bound
      val (move, keep) = args.partition(_.free disjoint bound)
      val bind_ = Exists(formals, And.nary(keep))
      val bind__ = __simplify(bind_, eqs, pos)
      val rest = assuming(bind__, eqs, con(move, _, pos))
      And(bind__ :: rest) */
    case Match(arg, cases) =>
      val _arg = rewrite(arg, eqs)
      val _cases = cases map {
        case Case(pat, body) =>
          val eq = pat.toExpr === arg
          Case(pat, assuming(eq, eqs, simplify(body, _, pos)))
      }
      Match(_arg, _cases)
    case bind: Bind =>
      __simplify(bind, eqs, pos)
    case _ =>
      rewrite(phi, eqs)
  }

  def __simplify(bind: Expr, eqs: Map[Id, Expr], pos: Boolean): Expr = bind match {
    case bind: Bind =>
      val Bind(quant, formals, body) = bind.refresh
      val bound = Set(formals map (_.id): _*)
      val _body = binding(formals, simplify(body, eqs, pos))
      val __body = prune(_body, quant, bound, pos)
      quant(formals, __body)
    case _ =>
      _simplify(bind, eqs, pos)
  }

  def rewrite(expr: Expr, eqs: Map[Id, Expr]): Expr = expr match {
    case id: Id if (eqs contains id) =>
      eqs(id)
    case Eq(left, right) =>
      eq(rewrite(left, eqs), rewrite(right, eqs))
    case Ite(test, left, right) =>
      Ite(rewrite(test, eqs), rewrite(left, eqs), rewrite(right, eqs))
    case Select(array, index) =>
      Select(rewrite(array, eqs), rewrite(index, eqs))
    case Store(array, index, value) =>
      Store(rewrite(array, eqs), rewrite(index, eqs), rewrite(value, eqs))
    case Distinct(args) =>
      Distinct(args map (rewrite(_, eqs)))
    case App(Id.head, List(App(Id.cons, List(head, tail)))) =>
      head
    case App(Id.tail, List(App(Id.cons, List(head, tail)))) =>
      tail
    case App(fun, args) =>
      App(fun, args map (rewrite(_, eqs)))
    case _ =>
      expr
  }

  def isTrue(phi: Expr) = {
    val res = (phi == True) || (backend isUnsat !phi)
    res
  }

  def isFalse(phi: Expr) = {
    val res = (phi == False) || (backend isUnsat phi)
    res
  }

  def binding[A](formals: List[Formal], a: => A): A = {
    backend.scoped {
      for (Formal(id, typ) <- formals)
        backend.declare(id, List(), typ)
      a
    }
  }

  def assuming[A](phis: List[Expr], eqs: Map[Id, Expr], a: Map[Id, Expr] => A): A = phis match {
    case Nil =>
      a(eqs)
    case phi :: rest =>
      assuming(phi, eqs, assuming(rest, _, a))
  }

  def assuming[A](phi: Expr, eqs: Map[Id, Expr], a: Map[Id, Expr] => A): A = {
    backend.scoped {
      backend.assert(phi)

      phi match {
        case Eq(x: Id, e) if !(e.free contains x) =>
          a(eqs + (x -> e))
        case Eq(e, x: Id) if !(e.free contains x) =>
          a(eqs + (x -> e))
        case And.nary(args) =>
          assuming(args, eqs, a)
        case _ =>
          a(eqs)
      }
    }
  }

  def prune(phi: Expr, quant: Quant, bound: Set[Id], pos: Boolean): Expr = phi match {
    case Eq(x: Id, e) if !(e.free contains x) && (bound contains x) =>
      if (!pos && quant == Exists || pos && quant == Forall) {
        True
      } else {
        phi
      }
    case Eq(e, x: Id) if !(e.free contains x) =>
      prune(x === e, quant, bound, pos)
    case Not(psi) =>
      val _psi = prune(psi, quant, bound, !pos)
      not(_psi)
    case Imp(phi, psi) =>
      val _phi = prune(phi, quant, bound, !pos)
      val _psi = prune(psi, quant, bound, pos)
      imp(_phi, _psi)
    case Or.nary(args) =>
      val _args = args map (prune(_, quant, bound, pos))
      or(_args)
    case And.nary(args) =>
      val _args = args map (prune(_, quant, bound, pos))
      and(_args)
    case _ =>
      phi
  }

  def eq(left: Expr, right: Expr): Expr = {
    if (left == right) True
    else Eq(left, right)
  }

  def not(phi: Expr) = phi match {
    case False => True
    case True => False
    case App(Id.not, List(phi)) => phi
    case _ => !phi
  }

  def imp(phi: Expr, psi: Expr) = (phi, psi) match {
    case (False, _) => True
    case (True, _) => psi
    case (_, False) => not(phi)
    case (_, True) => True
    case _ => phi ==> psi
  }

  def and(phi: Expr, psi: Expr) = (phi, psi) match {
    case (False, _) => False
    case (True, _) => psi
    case (_, False) => False
    case (_, True) => phi
    case _ => phi && psi
  }

  def or(phi: Expr, psi: Expr) = (phi, psi) match {
    case (False, _) => psi
    case (True, _) => True
    case (_, False) => phi
    case (_, True) => True
    case _ => phi || psi
  }

  def and(args: List[Expr]) = {
    if (args contains False) False
    else And(args filter (_ != True))
  }

  def or(args: List[Expr]) = {
    if (args contains True) True
    else Or(args filter (_ != False))
  }
}