package cuvee

object Simplify {
  case object Unsat extends Exception

  val backend = Solver.z3(100)

  def simplify(exprs: List[Expr]): List[Expr] = exprs match {
    case Nil =>
      Nil
      
    case (bind: Bind) ::rest =>
      val _phi = bind // don't simplify axioms
      val _rest = assuming(_phi, simplify(rest))
      _phi :: _rest

    case phi :: rest =>
      val _phi = simplify(phi)
      val _rest = assuming(_phi, simplify(rest))
      _phi :: _rest
  }

  def simplify(phi: Expr): Expr = {
    val _phi = _simplify(phi)
    /* println("simplify: ")
    println("  solver: " + Printer.solver(backend))
    println("     " + phi)
    println("  ~> " + _phi)
    println() */
    _phi
  }

  def _simplify(phi: Expr): Expr = phi match {
    case _ if isFalse(phi) =>
      False
    case _ if isTrue(phi) =>
      True
    case App(Id.not, List(phi)) =>
      val _phi = simplify(phi)
      not(_phi)
    case App(Id.and, List(phi, psi)) =>
      val _phi = simplify(phi)
      val _psi = assuming(_phi, simplify(psi))
      and(_phi, _psi)
    case App(Id.or, List(phi, psi)) =>
      val _phi = simplify(phi)
      val _psi = assuming(!_phi, simplify(psi))
      or(_phi, _psi)
    case App(Id.imp, List(phi, psi)) =>
      val _phi = simplify(phi)
      val _psi = assuming(_phi, simplify(psi))
      imp(_phi, _psi)
    case Bind(quant, formals, body) =>
      val _body = binding(formals, simplify(body))
      Bind(quant, formals, _body)
    case _ =>
      phi
  }

  def isTrue(phi: Expr) = {
    (phi == True) || (backend isUnsat !phi)
  }

  def isFalse(phi: Expr) = {
    (phi == False) || (backend isUnsat phi)
  }

  def binding[A](formals: List[Formal], a: => A): A = {
    backend.scoped {
      for (Formal(id, typ) <- formals)
        backend.declare(id, List(), typ)
      a
    }
  }

  def assuming[A](phi: Expr, a: => A): A = {
    backend.scoped {
      backend.assert(phi)
      a
    }
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
}