package cuvee

case class Simplify(state: State) {
  val backend = Solver.default
  state replay backend

  import Simplify._

  def apply(phis: List[Expr]): List[Expr] = {
    val _phis = nnf(phis)

    if (Simplify.debug) {
      println("input formulas:")
      for (phi <- phis)
        println(Printer.format(phi, "  "))
      println("negation normal form:")
      for (phi <- _phis)
        println(Printer.format(phi, "  "))
    }
    val _res = top(_phis)
    And.flatten(_res)
  }

  def top(args: List[Expr]): List[Expr] = {
    nary(todo = args, rdone = Nil, neg = false, top = true)
  }

  def con(args: List[Expr]): List[Expr] = {
    nary(todo = args, rdone = Nil, neg = false, top = false)
  }

  def dis(args: List[Expr]): List[Expr] = {
    nary(todo = args, rdone = Nil, neg = true, top = false)
  }

  def _assert(phi: Expr, neg: Boolean) = {
    if (neg)
      backend.assert(!phi)
    else
      backend.assert(phi)
  }

  def nary(todo: List[Expr], rdone: List[Expr], neg: Boolean, top: Boolean, changed: Boolean = false): List[Expr] = todo match {
    case Nil =>
      val done = rdone.reverse
      if (changed) nary(done, Nil, neg, top)
      else done

    // don't simplify top-level axioms (rarely useful)
    case (phi @ Forall(_, _)) :: rest if top =>
      assert(!neg)
      nary(rest, phi :: rdone, neg, top, changed)

    case phi :: rest =>
      val _phi = backend.scoped {
        for (ctx <- rest) _assert(ctx, neg)
        for (ctx <- rdone) _assert(ctx, neg)
        simplify(phi)
      }

      nary(rest, _phi :: rdone, neg, top, _phi != phi || changed)
  }

  def simplify(phi: Expr): Expr = {
    val (ms, _phi) = time(_simplify(phi))
    if (Simplify.debug) {
      println(s"simplify (${ms}ms)")
      println("     " + phi)
      println("  ~> " + _phi)
      println()
    }
    _phi
  }

  def _simplify(phi: Expr): Expr = phi match {
    case _ if isFalse(phi) =>
      False
    case _ if isTrue(phi) =>
      True
    case And.nary(args) =>
      val _args = con(args)
      and(_args)
    case Or.nary(args) =>
      val _args = dis(args)
      or(_args)
    case _ =>
      phi
  }

  def isTrue(phi: Expr) = {
    val res = (phi == True) || (backend isUnsat !phi)
    res
  }

  def isFalse(phi: Expr) = {
    val res = (phi == False) || (backend isUnsat phi)
    res
  }
}

object Simplify {
  var debug = false

  def not(phi: Expr): Expr = phi match {
    case True => False
    case False => True
    case Not(psi) => psi
    case _ => Not(phi)
  }

  def and(args: List[Expr]): Expr = {
    val _args = And.flatten(args)
    if (_args contains False) False
    else And(_args.distinct filter (_ != True))
  }

  def or(args: List[Expr]): Expr = {
    val _args = Or.flatten(args)
    if (_args contains True) True
    else Or(_args.distinct filter (_ != False))
  }

  def nnf(phi: Expr): Expr = phi match {
    case Not(True) =>
      False
    case Not(False) =>
      True
    case Not(Not(phi)) =>
      nnf(phi)
    case Not(Imp(phi, psi)) =>
      nnf(phi && !psi)
    case Not(And.nary(args)) =>
      or(nnf(Not(args)))
    case Not(Or.nary(args)) =>
      and(nnf(Not(args)))
    case Not(Bind(quant, formals, body)) =>
      Bind(!quant, formals, nnf(!body))

    case Imp(phi, psi) =>
      nnf(!phi || psi)
    case And.nary(args) =>
      and(nnf(args))
    case Or.nary(args) =>
      or(nnf(args))
    case Bind(quant, formals, body) =>
      Bind(quant, formals, nnf(body))

    case _ =>
      phi
  }

  def nnf(phis: List[Expr]): List[Expr] = {
    phis map nnf
  }
}
